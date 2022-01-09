Alright, what you can see in the graphs is the following:

- As the sample length $n$ gets larger or the underlying standard deviation $\sigma$
becomes smaller, the intervals tend to be shorter (assuming all else remains the
same, of course).
Intuitively, this makes sense as increasing $n$ gives us more information and decreasing
$\sigma$ reduces the spread of the observations. 
Both actions lower uncertainty which yields shorter confidence intervals.
Formally, we can see this by computing the length of the intervals from the above 
formula as
$$
(c_1 - c_2) \frac{\sigma}{n}.
$$

- As the number of observed samples $N$ increases, the frequency 
with which the real value of $\mu$ lies within the intervals is closer to the 
theoretical value $\alpha_2 - \alpha_1$ (see also next point). 
Let me stress here that $\mu$ does not change (if you leave the respective slider 
as it is) and the intervals are what changes. 
In our line of thinking $\mu$ is a constant and the interval bounds are what's 
random.
For completeness' sake, let me mention that this line of thinking is known as the
[frequentist's approach](https://en.wikipedia.org/wiki/Frequentist_probability) and 
is contrasted by the [Bayesian approach](https://en.wikipedia.org/wiki/Bayesian_probability)
where $\mu$ can be a random quantity.

- Here, the values $c_1$ and $c_2$ are given by the $\alpha_1$ and $\alpha_2$ quantiles of the
standard normal distribution.
Consequently, if both, $\alpha_1$ and $\alpha_2$, have an equal distance to $0.5$, then
the confidence intervals are centered around the sample mean.
This is due to the symmetry of the standard normal distribution which implies that 
$$
z_{\alpha} = - z_{1 - \alpha}
$$
where $z_{\alpha}$ is the $\alpha$-quantile of the standard normal distribution.
But, of course, you can favor one side over the over instead of centering the interval 
around the sample mean by choosing $\alpha_1$ and $\alpha_2$ asymmetrically.

So, we have learned how to construct a confidence interval for an unknown mean parameter
$\mu$ of a normally distributed sample if its standard deviation $\sigma$ is known.
Clearly, if instead $\sigma$ is also unknown, then we cannot use any of the previous
formulas to compute confidence intervals.
But what happens if we try to compensate our lack of knowledge of $\sigma$ by replacing it 
in all formulas with an empiricial pendant, say the empirical standard deviation
$$
S_n = \sqrt{\frac{1}{n - 1} \sum_{k = 1}^n (X_k - \overline{X}_n)^2}.
$$

Sadly, this would work only in the sense that now we can compute stuff but theoretically
this is not exactly correct.
Remember that we used the fact that our statistic $T$ is normally distributed?
Well, replacing $\sigma$ by $S_n$ forms a new statistic
$$
T = \sqrt{n}\frac{\overline{X}_n - \mu}{S_n}
$$
that is not normally distributed.
Consequently, we cannot use the quantiles of the standard normal distribution to 
find interval bounds.
Fortunately, this statistic follows a different distribution known as a 
[t-distribution](https://en.wikipedia.org/wiki/Student%27s_t-distribution) such that
we can use the quantiles of that distribution to form confidence intervals.

In summary, we can come up with confidence intervals by a couple of steps:

1. Find a statistic $T$ that contains only quantities you are interested in (e.g. $\mu$)
or know or can compute (e.g. $\overline{X}_n$).
2. Make sure that you know the distribution of said statistic.
3. Use the quantiles of the statistic's distribution to find constants such that 
$\mathbb{P}(c_1 \leq T \leq c_2) = 1 - \alpha \in (0, 1)$.
4. Rewrite the probability such that the parameter of interest is "in the middle".