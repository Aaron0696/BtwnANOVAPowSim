# BtwnANOVAPowSim
### Between-subjects ANOVA Power Simulator
> Implementing a simulation-approach to calculating power in R Shiny.

## Introduction
This is a personal project to consoliate and apply my knowledge in making R Shiny apps. The main objective of the app is to provide a *user-friendly* platform for users to simulate power while adjusting various parameters that are constrained by assumptions when using formulas to calculate power.

* For example, users can select different variances for each condition, whereas typical power calculation assumes that variances are similar across conditions.
* Users can also allow different groups to have different sizes to ascertain the effect of unequal group sizes.

## Power
> The probability that an effect would be statistically significant *in the sample* if the effect truly exists in the *population*.

Power is a concept that was taught in my first statistical course in Psychology. However, I felt that it did not receive its deserved attention during my undergraduate and graduate years in NUS. Few would conduct apriori power analysis while some might occasionally conduct post-hoc power analysis to justify why their experiment did not produce statistically significant results. From my personal experience, some reasons that contribute to this phenomenon are:

* Low stakes of projects (most experiments were simply used to attain a grade for a percentage of a course).
* Power-determinism, I came up with this term myself and it refers to the cognition that sample size is the only determinant of power that the experimenter has power over. So power is ignored with the heuristic: "Just collect as many subjects as possble."
* Not knowing how to calculate power.

The second and third reasons will hopefully be addressed (somewhat) by the app. The current version of the app focuses on one-way and two-way between-subjects ANOVA as these are common analysis methods amongst Psychology undergraduates. Depending on feedback, I may consider extending them to other analysis methods.

## Using The App

![image info](./pictures/appIntro.png)

### Launch

### Step One: General Settings

### Step Two: Condition Parameters

### Step Three: Run

### Output


