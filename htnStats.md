Cold Weather and Antihypertensive Drug Use
================
Manasa
Jan 3, 2017

<br> <br> **Part1: Obtaining Prescription Data** Read in Medicaid state utilization data of 2015

``` r
#CMS Medicaid State Utilization Data 2015
url="https://data.medicaid.gov/api/views/ju2h-vcgs/rows.csv?accessType=DOWNLOAD"
cmsdata <- read.csv(url) 
#head(cmsdata)
mycmsdata <- cmsdata
```

<br> Clean data

``` r
#Remove rows with null drug name 
mycmsdata <- cmsdata[!is.na(cmsdata$Product.Name),]
#Remove leading and trailing whitespace from drug name
mycmsdata$Product.Name <- gsub("^\\s+|\\s+$", "", mycmsdata$Product.Name)
#Remove row with blank drug name
mycmsdata <- mycmsdata[!(mycmsdata$Product.Name==""),]
#Drug name to uppercase
mycmsdata$Product.Name <- toupper(mycmsdata$Product.Name)
#Subset first ten characters of drug name
mycmsdata$Product.Name <- substr(mycmsdata$Product.Name, 0, 10)
```

<br> <br> **Part 2: Building a List of Drugs Indicated for Hypertension**

Solution 1: Using openFDA API

``` r
#Get list of drugs indicated for blood pressure from Open FDA API
#url = 'https://api.fda.gov/drug/event.json?search=patient.drug.drugindication:hypertension&count=patient.drug.openfda.generic_name.exact&limit=100'
#rawdata <- readLines(url, warn="F") 
#data <- jsonlite::fromJSON(rawdata)
#head(data)
#results <- data$results
#bpDrugs <- results$term
```

Result - As you can see from the drug list above, openFDA was innacurate as several drugs that are not actually indicated for hypertension are were listed

So... <br> Solution 2:

I created my own list of antihypertensive drugs using Micromedex's list of generic drugs that are effective and evidence shows efficacy in treating hypertension and supplemented it with brand names from WebMD's Common Drugs and Medications to treat High Blood Pressure.

Read in data

    ## 'data.frame':    268 obs. of  1 variable:
    ##  $ drug: Factor w/ 267 levels "accupril","accuretic",..: 3 10 11 14 12 15 16 13 18 19 ...

<br> Clean HTN drug names data

``` r
bpDrugs <- mydf$drug
#convert drug names to all uppercase so it is easier to compare with CMS data
BPdrugs <- toupper(bpDrugs)
#take first 10 character substrings of drug names so drug names are same format as CMS data
shortBPdrugs <- substr(BPdrugs, 0, 10)
#remove trailing white space from drug names
bpFinalList <- sub("\\s+$", "", shortBPdrugs)
```

<br> <br>
**Part 3: Data Manipulation**

``` r
#Get prescription data stats for HTN drugs
subdata <- subset(mycmsdata, mycmsdata$Product.Name %in% bpFinalList, select=c(Product.Name, Quarter, State, Units.Reimbursed, Number.of.Prescriptions, Total.Amount.Reimbursed))
#Format relevant data
bpVals <- aggregate(subdata$Number.of.Prescriptions~subdata$Quarter, FUN = function(x) c=sum(x))
totVals <- aggregate(mycmsdata$Number.of.Prescriptions~mycmsdata$Quarter, FUN = function(x) c=sum(x))
names(bpVals) <- c("Quarter", "Num.of.Prescriptions")
names(totVals) <- c("Quarter", "Num.of.Prescriptions")
nonBpVals <- cbind.data.frame(bpVals$Quarter, totVals$Num.of.Prescriptions - bpVals$Num.of.Prescriptions)
names(nonBpVals) <- c("Quarter", "Num.of.Prescriptions")
plotData <- merge(bpVals, nonBpVals, by="Quarter")
names(plotData) <- c("Quarter","BP","nonBP")
total <- merge(plotData, totVals, by="Quarter")
names(total) <- c("Quarter","BP", "nonBP", "Total")

#Find proportion of total drugs written for hypertension
percentage <- round(rbind.data.frame((total$BP[1]/total$Total[1])*100, (total$BP[2]/total$Total[2])*100,(total$BP[3]/total$Total[3])*100, (total$BP[4]/total$Total[4])*100, (total$nonBP[1]/total$Total[1])*100, (total$nonBP[2]/total$Total[2])*100,(total$nonBP[3]/total$Total[3])*100, (total$nonBP[4]/total$Total[4])*100), digits = 2)

#Reformat data to plot
require(reshape)
```

    ## Loading required package: reshape

    ## Warning: package 'reshape' was built under R version 3.2.5

``` r
mdata <- melt(plotData, id=c("Quarter"))
ndata <- cbind(mdata,percentage)
names(ndata) <- c("Quarter", "variable", "value", "percent")
```

<br> <br> **Part 4: Visualizing Data**

Bar plot showing total number of antihypertensive prescriptions per quarter. Note the quarter start dates are - Quarter 1: 1/1, Quarter 2: 4/1, Quarter 3: 7/1, Quarter 4: 10/1

``` r
require(ggplot2)
```

    ## Loading required package: ggplot2

``` r
ggplot(bpVals, aes(x=Quarter, y=Num.of.Prescriptions)) + geom_bar(stat="identity") + ggtitle("National Medicaid Antihypertensives Prescription Data") + labs(x="Quarter", y="Number of Prescriptions")
```

![](htnStats_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
myPlot <- ggplot() + geom_bar(aes(y = value, x = Quarter, fill=variable), data = ndata,
                                stat="identity") + geom_text(data=ndata, aes(x=Quarter, y=value,label=paste0(percent,"%"), position="stack")) +
  ggtitle("National Medicaid Prescription Data") + labs(x="Quarter", y="Number of Prescriptions")
myPlot
```

![](htnStats_files/figure-markdown_github/unnamed-chunk-9-1.png)

The percent of prescriptions for hypertension drugs seems to be similar among all for quarters using national data. However, not all states experience a large decrease in temperatures during winter months. So, I will narrow data and choose one state,Massachussetts, which has much colder temperatures in the winter, to see if there is an increase in antihypertensive use. <br>

``` r
#Subset of only MA state Medicaid utiliztion data for 2015
mycmsdata <- subset(cmsdata,cmsdata$State=="NY", select = c(Product.Name, Quarter, State, Units.Reimbursed, Number.of.Prescriptions, Total.Amount.Reimbursed))
```

I will repeat the data manipulation and visualization steps of the national data to produce plots for MA data. ![](htnStats_files/figure-markdown_github/unnamed-chunk-11-1.png)![](htnStats_files/figure-markdown_github/unnamed-chunk-11-2.png)