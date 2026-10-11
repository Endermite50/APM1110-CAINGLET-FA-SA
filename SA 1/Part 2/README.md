# Section II: Distribution Fitting and Normality Testing of Crypto Returns

This repository/document contains the empirical analysis of distribution fitting for Bitcoin (BTC/USD) and normality testing for Ethereum (ETH) returns.

## Key Findings

* **Bitcoin (BTC/USD) 1-Minute Returns:**
* Evaluated eight candidate distributions via maximum likelihood estimation.


* The **generalized normal distribution** fits best (ranking first in AIC, BIC, and KS distance), outperforming the Johnson SU and Student-t distributions.


* Exhibits a sharp peak at zero and heavy tails (shape parameter ~0.54).




* **Ethereum (ETH) 5-Minute Returns:**
* Normality is decisively rejected across all 200 of 5000 random samples ($p < 0.05$, mean $W = 0.779$) using the Shapiro-Wilk test.


* Supported by the Jarque-Bera test ($3.45 \times 10^8, p < 0.001$).


* Returns are nearly symmetric (skewness -0.16) but feature extreme fat tails (excess kurtosis of 93.4).
