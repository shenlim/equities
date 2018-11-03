# Black-Scholes-Merton Model (BSM) - Nov 2018. Author: Shen Lim.
# Copyright 2018, Shen Lim, All Rights Reserved.
import numpy as np
import scipy.stats as si

'''
Black-Scholes-Merton Model for European Options

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
    r = float(input("Risk-free rate (decimals)? "))
    sigma = float(input("Volatility (decimals)? "))
    call = input("Call or put? ")
    q = float(input("Dividends? Enter 0 if none. "))

    if (q == 0): # No dividends.
        
        d1 = (np.log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
        d2 = (np.log(S / K) + (r - 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
        N1 = si.norm.cdf(d1, 0.0, 1.0) # N(.) is the cumulative distribution function of the standard normal distribution.
        N2 = si.norm.cdf(d2, 0.0, 1.0)
        n1 = si.norm.cdf(-d1, 0.0, 1.0)
        n2 = si.norm.cdf(-d2, 0.0, 1.0)
        nPrime = np.exp((n1 ** 2)/2) / np.sqrt(2 * np.pi) # Standard normal probability density function.

        gamma = nPrime / (S * sigma * np.sqrt(T))
        vega = nPrime * S * np.sqrt(T)

        if (call.lower() == "c") or (call.lower() == "call"):
            call = (S * N1 - K * np.exp(-r * T) * N2)
            cDelta = N1
            cTheta = -((S * nPrime * sigma)/(2 * np.sqrt(T))) - (r * K * np.exp(-r * T) * N2)
            cRho = K * T * np.exp(-r * T) * N2
            
            print("Price of Call: $", round(call, 4))
            print("   Call Delta:  ", round(cDelta, 4))
            print("        Gamma:  ", round(gamma, 4))
            print("         Vega:  ", round(vega, 4))
            print("   Call Theta:  ", round(cTheta, 4))
            print("     Call Rho:  ", round(cRho, 4))

        elif (call.lower() == "p") or (call.lower() == "put"):
            put = (K * np.exp(-r * T) * n2 - S * n1)
            pDelta = N1 - 1
            pTheta = -((S * nPrime * sigma)/(2 * np.sqrt(T))) + (r * K * np.exp(-r * T) * n2)
            pRho = -K * T * np.exp(-r * T) * n2
            
            print("Price of Put: $", round(put, 4))
            print("   Put Delta:  ", round(pDelta, 4))
            print("       Gamma:  ", round(gamma, 4))
            print("        Vega:  ", round(vega, 4))
            print("   Put Theta:  ", round(pTheta, 4))
            print("     Put Rho:  ", round(pRho, 4))

        else:
            return "Invalid input."

    elif (q > 0): # Dividends.

        d1 = (np.log(S / K) + (r - q + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
        d2 = (np.log(S / K) + (r - q - 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
        N1 = si.norm.cdf(d1, 0.0, 1.0)
        N2 = si.norm.cdf(d2, 0.0, 1.0)
        n1 = si.norm.cdf(-d1, 0.0, 1.0)
        n2 = si.norm.cdf(-d2, 0.0, 1.0)
        nPrime = np.exp((n1 ** 2)/2) / np.sqrt(2 * np.pi)

        gamma = nPrime / (S * sigma * np.sqrt(T)) * np.exp(-q * T)
        vega = nPrime * S * np.sqrt(T) * np.exp(-q * T)

        if (call.lower() == "c") or (call.lower() == "call"):
            call = (S * np.exp(-q * T) * N1 - K * np.exp(-r * T) * N2)
            cDelta = N1 * np.exp(-q * T)
            cTheta = -((S * nPrime * sigma * np.exp(-q * T))/(2 * np.sqrt(T))) - (r * K * np.exp(-r * T) * N2) + (q * S * np.exp(-q * T) * N1)
            cRho = K * T * np.exp(-r * T) * N2
            
            print("Price of Call: $", round(call, 4))
            print("   Call Delta:  ", round(cDelta, 4))
            print("        Gamma:  ", round(gamma, 4))
            print("         Vega:  ", round(vega, 4))
            print("   Call Theta:  ", round(cTheta, 4))
            print("     Call Rho:  ", round(cRho, 4))

        elif (call.lower() == "p") or (call.lower() == "put"):
            put = (K * np.exp(-r * T) * n2 - S * np.exp(-q * T) * n1)
            pDelta = (N1 - 1) * np.exp(-q * T)
            pTheta = -((S * nPrime * sigma * np.exp(-q * T))/(2 * np.sqrt(T))) + (r * K * np.exp(-r * T) * n2) - (q * S * np.exp(-q * T) * n1)
            pRho = -K * T * np.exp(-r * T) * n2
            
            print("Price of Put: $", round(put, 4))
            print("   Put Delta:  ", round(pDelta, 4))
            print("       Gamma:  ", round(gamma, 4))
            print("        Vega:  ", round(vega, 4))
            print("   Put Theta:  ", round(pTheta, 4))
            print("     Put Rho:  ", round(pRho, 4))

        else:
            return "Invalid input."

    else:
        return "Invalid dividends."
