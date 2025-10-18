A problem we R users have when applying the did package is extracting the Pre-Average values. The STATA package does these calculations on its own.

Although extracting the average estimated values is somewhat direct and easy (calculating just the average mean), the same cannot be affirmed about the Standard Error. From the beginning, the Covariance-Variance Matrix is not easily accessible after aggregating the estimation utilizing the aggte() command

Therefore, here I tried to recreate the process so that any R users can exhibit the Pre-Average values of their Doubly Robust Difference-in-Difference.
