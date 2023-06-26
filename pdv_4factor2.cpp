// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp14)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath> 
#include <algorithm> 
#include <ql/quantlib.hpp>
//#include <RcppEigen.h>

using namespace Rcpp;
using namespace QuantLib;
using namespace std;
//using namespace Eigen;

// Taken from Dirk's rquantlib/src/vanilla.cpp : 
// https://github.com/eddelbuettel/rquantlib/blob/016822116c3d9216a36e4c0b69b6f7a405e86d47/src/vanilla.cpp
QuantLib::Option::Type getOptionType(const std::string &type) {
  QuantLib::Option::Type optionType;
  if (type=="call") {
    optionType = QuantLib::Option::Call;
  } else if (type=="put") {
    optionType = QuantLib::Option::Put;
  } else {
    Rcpp::stop(std::string("Unknown option ") + type);
  }
  return optionType;
}

QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure>
  flatRate(const QuantLib::Date& today,
           const QuantLib::ext::shared_ptr<QuantLib::Quote>& forward,
           const QuantLib::DayCounter& dc) {
    return QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure>(new QuantLib::FlatForward(today, QuantLib::Handle<QuantLib::Quote>(forward), dc));
}

QuantLib::ext::shared_ptr<QuantLib::BlackVolTermStructure>
  flatVol(const QuantLib::Date& today,
          const QuantLib::ext::shared_ptr<QuantLib::Quote>& vol,
          const QuantLib::DayCounter& dc) {
    return QuantLib::ext::shared_ptr<QuantLib::BlackVolTermStructure>(
      new QuantLib::BlackConstantVol(today,
                                     QuantLib::NullCalendar(),
                                     QuantLib::Handle<QuantLib::Quote>(vol), dc));
}

// [[Rcpp::export]]
Rcpp::List americanOptionEngine(std::string type,
                                double underlying,
                                double strike,
                                double dividendYield,
                                double riskFreeRate,
                                double maturity,
                                double volatility,
                                int timeSteps,
                                int gridPoints,
                                std::string engine,
                                Rcpp::Nullable<Rcpp::NumericVector> discreteDividends,
                                Rcpp::Nullable<Rcpp::NumericVector> discreteDividendsTimeUntil) {
  
#ifdef QL_HIGH_RESOLUTION_DATE
  // in minutes
  boost::posix_time::time_duration length = boost::posix_time::minutes(boost::uint64_t(maturity * 360 * 24 * 60));
#else
  int length = int(maturity * 360 + 0.5); // FIXME: this could be better
  
#endif
  QuantLib::Option::Type optionType = getOptionType(type);
  
  // new framework as per QuantLib 0.3.5, updated for 0.3.7
  // updated again for 0.9.0, see eg test-suite/americanoption.cpp
  QuantLib::Date today = QuantLib::Date::todaysDate();
  QuantLib::Settings::instance().evaluationDate() = today;
  QuantLib::DayCounter dc = QuantLib::Actual360();
  QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> spot(new QuantLib::SimpleQuote(underlying));
  QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> qRate(new QuantLib::SimpleQuote(dividendYield));
  QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> qTS = flatRate(today,qRate,dc);
  QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> rRate(new QuantLib::SimpleQuote(riskFreeRate));
  QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> rTS = flatRate(today,rRate,dc);
  QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> vol(new QuantLib::SimpleQuote(volatility));
  QuantLib::ext::shared_ptr<QuantLib::BlackVolTermStructure> volTS = flatVol(today, vol, dc);
  
  bool withDividends = discreteDividends.isNotNull() && discreteDividendsTimeUntil.isNotNull();
  
#ifdef QL_HIGH_RESOLUTION_DATE
  QuantLib::Date exDate(today.dateTime() + length);
#else
  QuantLib::Date exDate = today + length;
#endif
  
  QuantLib::ext::shared_ptr<QuantLib::StrikedTypePayoff> payoff(new QuantLib::PlainVanillaPayoff(optionType, strike));
  QuantLib::ext::shared_ptr<QuantLib::Exercise> exercise(new QuantLib::AmericanExercise(today, exDate));
  
  QuantLib::ext::shared_ptr<QuantLib::BlackScholesMertonProcess>
    stochProcess(new QuantLib::BlackScholesMertonProcess(QuantLib::Handle<QuantLib::Quote>(spot),
                                                         QuantLib::Handle<QuantLib::YieldTermStructure>(qTS),
                                                         QuantLib::Handle<QuantLib::YieldTermStructure>(rTS),
                                                         QuantLib::Handle<QuantLib::BlackVolTermStructure>(volTS)));
  
  if (withDividends) {
    Rcpp::NumericVector divvalues(discreteDividends), divtimes(discreteDividendsTimeUntil);
    int n = divvalues.size();
    std::vector<QuantLib::Date> discDivDates(n);
    std::vector<double> discDividends(n);
    for (int i = 0; i < n; i++) {
#ifdef QL_HIGH_RESOLUTION_DATE
      boost::posix_time::time_duration discreteDividendLength = boost::posix_time::minutes(boost::uint64_t(divtimes[i] * 360 * 24 * 60));
      discDivDates[i] = QuantLib::Date(today.dateTime() + discreteDividendLength);
#else
      discDivDates[i] = today + int(divtimes[i] * 360 + 0.5);
#endif
      discDividends[i] = divvalues[i];
    }
    
    QL_DEPRECATED_DISABLE_WARNING
    QuantLib::DividendVanillaOption option(payoff, exercise, discDivDates, discDividends);
    QL_DEPRECATED_ENABLE_WARNING
    if (engine=="BaroneAdesiWhaley") {
      Rcpp::warning("Discrete dividends, engine switched to CrankNicolson");
      engine = "CrankNicolson";
    }
    
    if (engine=="CrankNicolson") { // FDDividendAmericanEngine only works with CrankNicolson
      // suggestion by Bryan Lewis: use CrankNicolson for greeks
      QuantLib::ext::shared_ptr<QuantLib::PricingEngine>
      fdcnengine(new QuantLib::FdBlackScholesVanillaEngine(stochProcess, timeSteps, gridPoints));
      option.setPricingEngine(fdcnengine);
      return Rcpp::List::create(Rcpp::Named("value") = option.NPV(),
                                Rcpp::Named("delta") = option.delta(),
                                Rcpp::Named("gamma") = option.gamma(),
                                Rcpp::Named("vega") = R_NaReal,
                                Rcpp::Named("theta") = R_NaReal,
                                Rcpp::Named("rho") = R_NaReal,
                                Rcpp::Named("divRho") = R_NaReal);
    } else {
      throw std::range_error("Unknown engine " + engine);
    }
    
  } else {
    QuantLib::VanillaOption option(payoff, exercise);
    
    if (engine=="BaroneAdesiWhaley") {
      // new from 0.3.7 BaroneAdesiWhaley
      
      QuantLib::ext::shared_ptr<QuantLib::PricingEngine> engine(new QuantLib::BaroneAdesiWhaleyApproximationEngine(stochProcess));
      option.setPricingEngine(engine);
      return Rcpp::List::create(Rcpp::Named("value") = option.NPV(),
                                Rcpp::Named("delta") = R_NaReal,
                                Rcpp::Named("gamma") = R_NaReal,
                                Rcpp::Named("vega") = R_NaReal,
                                Rcpp::Named("theta") = R_NaReal,
                                Rcpp::Named("rho") = R_NaReal,
                                Rcpp::Named("divRho") = R_NaReal);
    } else if (engine=="CrankNicolson") {
      // suggestion by Bryan Lewis: use CrankNicolson for greeks
      QuantLib::ext::shared_ptr<QuantLib::PricingEngine>
      fdcnengine(new QuantLib::FdBlackScholesVanillaEngine(stochProcess, timeSteps, gridPoints));
      option.setPricingEngine(fdcnengine);
      return Rcpp::List::create(Rcpp::Named("value") = option.NPV(),
                                Rcpp::Named("delta") = option.delta(),
                                Rcpp::Named("gamma") = option.gamma(),
                                Rcpp::Named("vega") = R_NaReal,
                                Rcpp::Named("theta") = R_NaReal,
                                Rcpp::Named("rho") = R_NaReal,
                                Rcpp::Named("divRho") = R_NaReal);
    } else {
      throw std::range_error("Unknown engine " + engine);
    }
  }
  
  
}


// Pricing function for American options with simulated volatility using pre-simulated paths
class BaroneAdesiWhaleyWorker : public RcppParallel::Worker {
public:
  BaroneAdesiWhaleyWorker(const Rcpp::NumericVector& spot,
                          const Rcpp::NumericVector& strike,
                          const Rcpp::NumericVector& riskFreeRate,
                          const Rcpp::NumericVector& dividendYield,
                          const Rcpp::NumericVector& volatility,
                          const Rcpp::NumericVector& timeToExpiry,
                          const Rcpp::LogicalVector& isCallOption,
                          Rcpp::NumericVector& optionPrices)
    : spot_(spot), strike_(strike), riskFreeRate_(riskFreeRate),
      dividendYield_(dividendYield), volatility_(volatility),
      timeToExpiry_(timeToExpiry), isCallOption_(isCallOption),
      optionPrices_(optionPrices) {}
   
  void operator()(std::size_t begin, std::size_t end) {
    // for each strike
    int len = spot_.size();
    for (std::size_t i = begin; i < end; ++i) {
      double strike = strike_[i];
      double riskFreeRate = riskFreeRate_[i];
      double dividendYield = dividendYield_[i];
      double timeToExpiry = timeToExpiry_[i];
      std::string type = isCallOption_[i] ? "call" : "put";
      double sum_option = 0;
      for(int j = 0; j < len; ++j) {
        double spot = spot_[j];
        double volatility = volatility_[j];
        double tmp_option = 0.;
        Rcpp::List optionValue = americanOptionEngine(type, spot, strike, dividendYield,
                                             riskFreeRate, timeToExpiry, volatility,
                                              150, 149, "BaroneAdesiWhaley", NULL, NULL);
        tmp_option = optionValue["value"];
        sum_option += tmp_option;
      }
      sum_option /= len;
       
      optionPrices_[i] = sum_option;
    }
  }
   
private:
  const Rcpp::NumericVector& spot_;
  const Rcpp::NumericVector& strike_;
  const Rcpp::NumericVector& riskFreeRate_;
  const Rcpp::NumericVector& dividendYield_;
  const Rcpp::NumericVector& volatility_;
  const Rcpp::NumericVector& timeToExpiry_;
  const Rcpp::LogicalVector& isCallOption_;
  Rcpp::NumericVector& optionPrices_;
};

// Wrapper to compute in parallel option value means of each paths given a serie of strikes
// [[Rcpp::export]]
Rcpp::NumericVector computeBaroneAdesiWhaleyOptionPriceParallel(Rcpp::NumericVector spot,
                                                                Rcpp::NumericVector strike,
                                                                Rcpp::NumericVector riskFreeRate,
                                                                Rcpp::NumericVector dividendYield,
                                                                Rcpp::NumericVector volatility,
                                                                Rcpp::NumericVector timeToExpiry,
                                                                Rcpp::LogicalVector isCallOption) {
  int n = strike.size();
  Rcpp::NumericVector optionPrices(n);
   
  BaroneAdesiWhaleyWorker worker(spot, strike, riskFreeRate, dividendYield,
                                 volatility, timeToExpiry, isCallOption,
                                 optionPrices);
  RcppParallel::parallelFor(0, n, worker);
  
  return optionPrices;
}

// Compute PDV vol
// [[Rcpp::export]]
NumericVector compute_vol(NumericVector R_1, NumericVector R_2, NumericVector betas,
                          double parabolic, double parabolic_offset) {
  NumericVector vol = betas[0] + betas[1] * R_1 + betas[2] * sqrt(R_2); //+ 
      //parabolic * pow(Rcpp::ifelse(R_1 > parabolic_offset, R_1 - parabolic_offset, 0.0), 2);
  return vol;
}

// Debug function to check values
// [[Rcpp::export]]
void printQuantiles(const std::string& name, Rcpp::NumericVector brownian_increment) {
  Rcpp::NumericVector probs = Rcpp::NumericVector::create(0.0, 0.25, 0.5, 0.75);
  Rcpp::Function quantileFunc("quantile");
  Rcpp::NumericVector quantiles = quantileFunc(Rcpp::wrap(brownian_increment), probs);
  Rcpp::Rcout << "Quantiles of " << name << ":" << std::endl;
  for (int i = 0; i < quantiles.size(); ++i) {
    Rcpp::Rcout << "Quantile " << probs[i] << ": " << quantiles[i] << std::endl;
  }
}


// Function to simulate PDV stock paths
// [[Rcpp::export]]
List simulate_cpp(int n_paths, double S0, NumericVector parameters, NumericVector R1_0, 
                  NumericVector R2_0, long double maturity, long double timestep_per_day) {
  
  // Set number of timesteps
  long double dt= 1. / 252.;
  long double timestep = dt / timestep_per_day;
  long double T = maturity / timestep;
  int n_timesteps = static_cast<int>(T);
  
  // Unpack parameters
  NumericVector betas(3);
  betas[0] = parameters[0];
  betas[1]  = parameters[1];
  betas[2]  = parameters[2];
  NumericVector lam1(2);
  lam1[0] = parameters[3];
  lam1[1] = parameters[4];
  long double theta1 = parameters[5];
  NumericVector lam2(2);
  lam2[0] = parameters[6];
  lam2[1] = parameters[7];
  long double theta2 = parameters[8];
  long double parabolic = parameters[9];
  long double parabolic_offset = parameters[10];

  // Convert R matrices to C++ matrices
  NumericVector r1_base(R1_0);
  NumericVector r2_base(R2_0);
  
  // Create output variables
  NumericMatrix vol_array(n_timesteps + 1, n_paths);
  NumericMatrix S_array(n_timesteps + 1, n_paths);
  
  // Set initial stock price
  NumericMatrix::Row S_row = S_array.row(0);
  std::fill(S_row.begin(), S_row.end(), S0);
  
  NumericVector R1(n_paths);
  NumericVector R2(n_paths);
  NumericVector vol(n_paths);
  NumericMatrix r1(2, n_paths);
  NumericMatrix r2(2, n_paths);
  NumericVector increment(n_paths);
  NumericVector brownian_increment(n_paths);
  NumericVector stock_increment(n_paths);
  
  // Simulate stock paths
  for (int t = 0; t < n_timesteps; t++) {
    brownian_increment = sqrt(timestep) * as<NumericVector>(rnorm(n_paths));
    if(t == 0) {
      long double R1_first = (1 - theta1) * r1_base[0] + theta1 * r1_base[1];
      long double R2_first = (1 - theta2) * r2_base[0] + theta2 * r2_base[1];
      long double vol = betas[0] + betas[1] * R1_first + betas[2] * std::sqrt(R2_first);
      
      NumericMatrix::Row vol_row = vol_array.row(0);
      std::fill(vol_row.begin(), vol_row.end(), vol);
      
      increment = vol * brownian_increment;
      
      for (int j = 0; j < 2; j++) {
        r1(j, _) = exp(-lam1[j] * timestep) * (r1_base[j] + lam1[j] * increment);
        NumericMatrix::Row r2_row = r2.row(j);
        long double tmp_row = exp(-lam2[j] * timestep) * (r2_base[j] + lam2[j] * pow(vol, 2) * timestep);
        std::fill(r2_row.begin(), r2_row.end(), tmp_row);
      }  
      /*
      printQuantiles("increment",increment);
      printQuantiles("brownian_increment",brownian_increment);
      printQuantiles("r1",r1);
      printQuantiles("r2",r2);*/
    } else{
      R1 = (1 - theta1) * r1(0, _) + theta1 * r1(1, _);
      R2 = (1 - theta2) * r2(0, _) + theta2 * r2(1, _);
      vol = compute_vol(R1, R2, betas, parabolic, parabolic_offset);
      
      vol_array(t, _) = vol;
      increment = vol * brownian_increment;
      
      for (int j = 0; j < 2; j++) {
        r1(j, _) = exp(-lam1[j] * timestep) * (r1[j] + lam1[j] * increment);
        r2(j, _) = exp(-lam2[j] * timestep) * (r2[j] + lam2[j] * pow(vol, 2) * timestep);
      }
      /*
      printQuantiles("brownian_increment",brownian_increment);
      printQuantiles("increment",increment);
      printQuantiles("R1",R1);
      printQuantiles("R2",R2);
      printQuantiles("vol",vol);
      printQuantiles("r1",r1);
      printQuantiles("r2",r2);
      */
    }
    stock_increment = exp(increment - 0.5 * pow(vol, 2) * timestep);
    //printQuantiles("stock_increment",stock_increment);
    S_array(t + 1, _) = S_array(t, _) * stock_increment;

  }
  R1 = (1 - theta1) * r1(0, _) + theta1 * r1(1, _);
  R2 = (1 - theta2) * r2(0, _) + theta2 * r2(1, _);
  vol = compute_vol(R1, R2, betas, parabolic, parabolic_offset);
  vol_array(n_timesteps, _) = vol;
  
  return List::create(Named("S_array") = S_array,
                      Named("vol_array") = vol_array,
                      Named("r1_array") = r1,
                      Named("r2_array") = r2
  );
}

/*
// [[Rcpp::export]]
List compute_cash_flows(NumericMatrix paths, double maturity, NumericVector strikes, double risk_free_rate) {
  int n_paths = paths.nrow();
  int n_steps = paths.ncol();
  int n_strikes = strikes.size();
  
  List cash_flows_list(n_strikes);
  
  for (int k = 0; k < n_strikes; k++) {
    double strike = strikes[k];
    
    NumericMatrix cash_flows(n_paths, n_steps);
    
    for (int i = 0; i < n_paths; i++) {
      double final_price = paths(i, n_steps - 1);
      double payoff = std::max(final_price - strike, 0.0);
      double discount_factor = std::exp(-risk_free_rate * maturity);
      
      for (int j = 0; j < n_steps; j++) {
        double price = paths(i, j);
        double time_to_maturity = maturity - (static_cast<double>(j) / n_steps) * maturity;
        double cash_flow = payoff * std::exp(-risk_free_rate * time_to_maturity);
        
        cash_flows(i, j) = cash_flow;
      }
    }
    
    cash_flows_list[k] = cash_flows;
  }
  
  return cash_flows_list;
}

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
NumericVector polynomial_regression(NumericVector x, NumericVector y, int degree) {
  // Create a design matrix with polynomial terms
  int n = x.size();
  MatrixXd X(n, degree + 1);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j <= degree; j++) {
      X(i, j) = pow(x[i], j);
    }
  }
  
  // Fit the regression model
  MatrixXd XtX = X.transpose() * X;
  MatrixXd Xty = X.transpose() * Map<VectorXd>(y.begin(), y.size());
  VectorXd coefficients = XtX.colPivHouseholderQr().solve(Xty);
  
  // Predict the values based on the model
  NumericVector predicted_values = wrap(X * coefficients);
  
  return predicted_values;
}

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
long double longstaff_schwartz_cpp(NumericMatrix paths, NumericMatrix cashflows, int degree) {
  int n_paths = paths.nrow();
  int n_steps = paths.ncol();
  
  // Initialize the exercise values matrix
  NumericMatrix exercise_values(n_paths, n_steps);
  
  // Iterate backwards through the time steps
  for (int i = n_steps - 1; i >= 0; i--) {
    // Update exercise values based on immediate payoffs
    for (int j = 0; j < n_paths; j++) {
      exercise_values(j, i) = cashflows(j, i);
    }
    
    // Perform Longstaff-Schwartz algorithm
    for (int j = i + 1; j < n_steps; j++) {
      NumericVector x = paths(_, j);
      NumericVector y = exercise_values(_, j);
      
      NumericVector continuation_values = polynomial_regression(x, y, degree);
      
      for (int k = 0; k < n_paths; k++) {
        double continuation_value = continuation_values[k];
        
        // Compare immediate payoff to continuation value
        if (cashflows(k, j) > continuation_value) {
          exercise_values(k, j) = cashflows(k, j);
        }
      }
    }
  }
  
  // Compute mean of the expected maturity
  NumericVector lastColumn = exercise_values(_, n_steps - 1);
  double columnSum = sum(lastColumn);
  double mean = columnSum / n_paths;
  return mean;
}

// [[Rcpp::export]]
NumericVector PDV_LS(int n_paths, double S0, NumericVector parameters, 
                     NumericVector R_init1, NumericVector R_init2, double maturity, 
                     NumericVector strikes, double risk_free_rate, 
                     int timestep_per_day, int degree) {
  List paths = simulate_cpp(n_paths, S0, parameters, R_init1, R_init2, maturity, timestep_per_day);
  List list = as<List>(paths); 
  NumericMatrix S_array = list["S_array"];
  NumericMatrix S_array_t = Rcpp::transpose(S_array);
  List cashflows = compute_cash_flows(S_array_t, maturity, strikes, risk_free_rate);
  int nCashflows = cashflows.size();
  NumericVector continuation_values(nCashflows);
  for (int i = 0; i < nCashflows; i++) {
    NumericMatrix cashflow = cashflows[i];
    long double continuation_value = longstaff_schwartz_cpp(S_array, cashflow, degree);
    continuation_values[i] = continuation_value;
  }
  return continuation_values;
}
*/