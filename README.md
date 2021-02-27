# Abstract
Restaurant businesses are suffering in the advent of COVID-19 lockdown restrictions. Within this paper, a randomized controlled trial was conducted in order to explore the effects of reopening dine-in services for restaurants within the GTA. It was discovered that restaurants that allow for dine-in services provide higher salaries and more job opportunities on average compared to restaurants that either offer take-out or delivery services only. Moreover, this analysis illustrated that expense structures between restaurants are likely to shift as a result of offering dine-in services during the pandemic, such that variable costs are likely to increase. Ultimately, restaurants that allow patrons to dine-in see a significant increase in revenue as a result (a difference of $5000 on average). By analyzing this data, results may help the government enact appropriate lockdown policies that take a nuanced consideration of restaurant businesses.

# Overview of the Data
This experiment was conducted by Petit Poll and was authorized by the Ontario Department of Public Health. A survey was distributed to restaurants across the GTA to analyze the business effects of reopening dine-in services to a randomly selected treatment group. 

The dataset includes features such as:
* `type` : Categorical identifier [“Treated” or “Control”] for each observation 
* `Q1` : First three digitals of the postcode 
* `Q2` : Categorical identifier for distinguishing the type of restaurants 
* `Q3` : Region name in GTA
* `Q4` : Describe whether the restaurant is a franchise (“Franchise” or “No”)
* `Q5` : The length of the operation years for each restaurant 
* `Q6` : Describe whether the restaurant offer takeout service (“Yes” or “No”)
* `Q7` : Describe whether the restaurant offer delivery service (“Yes” or “No”)
* `Q8` : Number of employees in the restaurant (category type)
* `Q9` : Average employee hourly rate (CAD) 
* `Q10` : Describe whether the restaurant has been a site of a potential COVID case
* `Q11` : Describe the restaurant’s fixed costs change situation 
* `Q12` :  Describe the restaurant’s flexible costs change situation 
* `Q13` : The restaurant’s past month revenue (CAD)

# File Structure
* inputs : any inputs used in the analysis of this report
  * data : includes the raw and the cleaned csv dataset
  * literature : any referenced articles, journals, or other literature
* outputs : outputs of the report
  * paper : includes the markdown script, pdf export, and bibtex reference file
* scripts : includes data import and cleaning scripts
