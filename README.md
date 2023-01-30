# ⛏️ Digging_For_Gold 💰

A Monte Carlo Simulation used to generate probabilities for the business to decide on different options and strategies. 

[![Made With](https://github.com/Denneya/linear_regression_industry_finance/blob/main/made-with-r.svg)](https://github.com/Denneya/linear_regression_industry_finance/blob/main/AT1A_24418042.R)

Author: Denneya Muscat

## Setting the Scene 🎬
A company requires a five-year strategy to maximise the chances of its long-term viability. The current situation of the company was outlined and minimum, most likely and maximum values of gold income, cost and new claim ventures were provided. There was also an extra legal challenge where the company could decide to resolve the case (and risk losing everything) or delay the case and pay a legal fee each year. The probability of resolving this case in the company's favour was 50% (to me, this wasn't enough to risk everything). The company also had the opportunity to invest money into acquiring a new claim, however the chance of discover was only 15%.

## Step 1: Calculations 🧮
To begin, the basic calculations had to be figured out over the five years in order for the Monte Carlo Simulation to be used. The data can be seen below:
![original data](https://github.com/Denneya/Digging_For_Gold/blob/main/Screenshot%202023-01-30%20at%205.46.12%20pm.png)

## Step 2: Decision Tree 🌳
For me, the first decision to make was to either delay or resolve the legal case. Different scenarios were outlined depending on this decision, and a Monte Carlo Simulation was run for each year depending on whether or not a future claim was successful/if the legal case was resolved in favour of the company. The scenarios I outlined can be seen in the decision tree below:
![decision tree](https://github.com/Denneya/Digging_For_Gold/blob/main/Decision%20Tree.drawio.pdf)

## Step 3: Decide on a Scenario and Support with Visuals 📊
By producing a line plot of all scenarios on the same graph, it was evident that they would all end up reaching around the same amount of profit at the end of the 5 years. To me, it was clear that I would recommend the scenario which has the least amount of risk and therefore chose Scenario 1. This could easily turn into Scenario 2 if the company were successful in acquiring a future claim. The line plot below shows the growth of each scenario (except for Scenario 4 which loses the legal case). The column graph below that compares Scenario's 1 & 2, the only different being Scenario 2 acquaries a future claim in year 2. 
![line plot](https://github.com/Denneya/Digging_For_Gold/blob/main/Screenshot%202023-01-30%20at%205.58.26%20pm.png)
![column graph](https://github.com/Denneya/Digging_For_Gold/blob/main/Screenshot%202023-01-30%20at%205.59.26%20pm.png)


## Final Comments
This task was very open to interpretation, which almost made it harder to begin. There are many strategies/options that could've been taken, however I chose to stick with the four options outlined above. 

