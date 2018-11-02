import numpy as np
import scipy.stats as si

'''
Black-Scholes-Merton Model

S: Spot stock price
K: Strike price
T: Time to maturity
r: Risk-free rate of interest (in decimals)
sigma: Stock volatility
q: Dividend rate (in decimals)
'''

def bs():
    S = float(input("Spot stock price? "))
    K = float(input("Strike price? "))
    T = float(input("Time to maturity (years)? "))
    r = float(input("Risk-free rate? "))
    sigma = float(input("Volatility (sigma)? "))
    call = input("Call or put? ")
    q = float(input("Dividends? Enter 0 if none. "))

    if (q == 0):
        
        d1 = (np.log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
        d2 = (np.log(S / K) + (r - 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))

        if (call.lower() == "c") or (call.lower() == "call"):
            call = (S * si.norm.cdf(d1, 0.0, 1.0) - K * np.exp(-r * T) * si.norm.cdf(d2, 0.0, 1.0))
            return call

        elif (call.lower() == "p") or (call.lower() == "put"):
            put = (K * np.exp(-r * T) * si.norm.cdf(-d2, 0.0, 1.0) - S * si.norm.cdf(-d1, 0.0, 1.0))
            return put

        else:
            return "Invalid input."

    elif (q > 0):

        d1 = (np.log(S / K) + (r - q + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
        d2 = (np.log(S / K) + (r - q - 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))

        if (call.lower() == "c") or (call.lower() == "call"):
            call = (S * np.exp(-q * T) * si.norm.cdf(d1, 0.0, 1.0) - K * np.exp(-r * T) * si.norm.cdf(d2, 0.0, 1.0))
            return call

        elif (call.lower() == "p") or (call.lower() == "put"):
            put = (K * np.exp(-r * T) * si.norm.cdf(-d2, 0.0, 1.0) - S * np.exp(-q * T) * si.norm.cdf(-d1, 0.0, 1.0))
            return put

        else:
            return "Invalid input."

    else:
        return "Invalid dividends."
