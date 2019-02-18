```
Exercise 5.6 What is the equation analogous to (5.6) for action values Q(s, a) instead of
state values V (s), again given returns generated using b?
```
> ![](figures/exercise_5-6.png "Exercise 5.6")

```
Exercise 5.7 In learning curves such as those shown in Figure 5.3 error generally decreases
with training, as indeed happened for the ordinary importance-sampling method. But for
the weighted importance-sampling method error first increased and then decreased. Why
do you think this happened?
```
> Weighted importance-sampling has bias towards Vb and Vb is a random policy, so its error increases for the initial episodes.

```
Exercise 5.8 The results with Example 5.5 and shown in Figure 5.4 used a first-visit MC
method. Suppose that instead an every-visit MC method was used on the same problem.
Would the variance of the estimator still be infinite? Why or why not?
```
> ![](figures/exercise_5-8.png "Exercise 5.8")

```
Exercise 5.10 Derive the weighted-average update rule (5.8) from (5.7). Follow the
pattern of the derivation of the unweighted rule (2.3).
```
> ![](figures/exercise_5-10.png "Exercise 5.10")
