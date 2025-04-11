## NYC Parking Violation Analysis

This is my course project in statistics program in UW-Madison. Thanks to my teammates [Hao Tong](https://github.com/htong25), [Junxia Zhao](https://github.com/jzhao347) and Jiayi Shen.

The data can be found on [NYC Department of Finance, NYC Open Data](https://data.cityofnewyork.us/City-Government/Parking-Violations-Issued-Fiscal-Year-2021/pvqr-7yc4).

# Introduction

Parking is pretty much a way of life in NYC. Meanwhile, parking violations have always been a great cause for traﬃc jam nowadays. Can we find some useful infomation to mitigate and help NYC government to make data-driven decisions? 
Below are some points that we are curious about:
- Is there a diﬀerence in violation amounts between weekdays and weekends? Any seasonality?
- Does plate registration state or vehicle body type have a relationship with parking violations?

And our workflow is:
- We program in **R** to conduct the student’s **t-test** to compare violation amounts between weekdays and weekends across all year.
- We apply **serial correlation** to analyze seasonal patterns.
- We utilize **data visualization** to show which registration state’s car and county cause more violations and defined the relationship between body type and violation code.

# Data Processing

The dataset contains **45 Million** parking tickets data issued from July 2014 to June 2018. There are tens of columns in each of the 4 huge files.

The total data is aobut 10.27 GB. In order to compute in a simple way and save time for a huge dataset, we decide to use **CHTC** to conduct parallel computation for data processing.

# Feature Selection & Engineering

We successfully reduce the dimensions and extract 5 useful and shared variables that are related to our scope. They are `Registration.State`, `Issue.Date`, `Violation.Code`,
`Vehicle.Body.Type`, and `Violation.County`.

# Conclusion


