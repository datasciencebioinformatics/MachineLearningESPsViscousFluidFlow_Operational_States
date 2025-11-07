# https://www.google.com/search?q=R+cran+%22add+instability%22+time-series&client=ubuntu-chr&sca_esv=fa0b78d7cd41bbf7&sxsrf=AE3TifObBoIltsaDpYX6C4zOdSvlRR9xjQ%3A1762538088397&ei=aDIOaZL7F7iN5OUP2f_ZwAE&ved=0ahUKEwiSz6WFzuCQAxW4BrkGHdl_FhgQ4dUDCBE&uact=5&oq=R+cran+%22add+instability%22+time-series&gs_lp=Egxnd3Mtd2l6LXNlcnAiJFIgY3JhbiAiYWRkIGluc3RhYmlsaXR5IiB0aW1lLXNlcmllczIFEAAY7wUyBRAAGO8FMgUQABjvBTIFEAAY7wUyBRAAGO8FSKUzUP8WWJcxcAJ4AZABAJgBrQGgAf0FqgEDMC41uAEDyAEA-AEBmAIGoAL1BMICChAAGLADGNYEGEeYAwCIBgGQBgiSBwMyLjSgB60SsgcDMC40uAfqBMIHAzItNsgHGw&sclient=gws-wiz-serp
# Simulate a time series with a constant mean of 10
ts_segment1 <- arima.sim(n = 50, model = list(ar = 0.5), mean = 10, sd = 1)

# Simulate a second segment with a different mean of 20 (a structural break)
ts_segment2 <- arima.sim(n = 50, model = list(ar = 0.5), mean = 20, sd = 2)

# Combine the segments into a single time series
unstable_ts <- ts(c(ts_segment1, ts_segment2), start = 1)

# Plot the result
plot(unstable_ts, main = "Time Series with a Structural Break", xlab = "Time", ylab = "Value")
abline(v = 50, col = "red", lty = 2) # Mark the break point
