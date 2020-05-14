 ##########################################
 #                                        #
###           ADM PRACTICAL CA           ###
 #                                        #
 ##########################################

# The key to success in any organization is attracting and retaining top talent. 
# You are an HR analyst at my company, and one of my tasks is to determine which factors 
# keep employees at my company and which prompt others to leave. We need to know what 
# factors we can change to prevent the loss of good people. 
 
# You have data about past and current employees in a spreadsheet. It has various data 
# points on our employees, but we're' most interested in whether they’re still with the 
# company or whether they’ve gone to work somewhere else. And we want to understand how 
# this relates to workforce attrition. 

#Attributes:
 # Age: in years
 # Attrition: Y/N the dependent variable -- have they left the company?
 # BusinessTravel: Non-Travel; Traval_Frequently, Travel_Rarely
 # DailyRate: Consultancy Charge per Day
 # Department: Human Resources; Research & Development; Sales
 # DistanceFromHome: How far the employe lives from work
 # Education: 1 'Below College'; 2 'College'; 3 'Bachelor'; 4 'Master'; 5 'Doctor'
 # EducationField: Human Resources; Life Sciences; Marketing; Medical; Other; Technical Degree
 # EmployeeCount: No of employes in this record	
 # EmployeeNumber: Employee ID
 # EnvironmentSatisfaction: 4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'	
 # Gender: Male / Female
 # HourlyRate: Consultancy Charge per Hour
 # JobInvolvement: 4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'
 # JobLevel	Metadata not available -- make an assumption 
 # JobRole: Healthcare Representative;  Human Resources; Laboratory Technician; Manager; Manufacturing Director; Research Director; Research Scientist; Sales Executive; Sales Representative 
 # JobSatisfaction: 4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'
 # MaritalStatus: Divorced; Married; Single
 # MonthlyIncome: monthly salary
 # MonthlyRate: Consultancy Charge per Day
 # NumCompaniesWorked: No. of previous employeers
 # Over18: Y/N
 # OverTime: Yes/No
 # PercentSalaryHike: Last Years Increment	
 # PerformanceRating:  4 point Likert scale: 1 'Low'; 2 'Good'; 3 'Excellent'; 4 'Outstanding'
 # RelationshipSatisfaction:  4 point Likert scale: 1 'Low'; 2 'Medium'; 3 'High'; 4 'Very High'
 # StandardHours: Contract hours	
 # StockOptionLevel: No available metadata -- make an assumption :)	
 # TotalWorkingYears: Career Age
 # TrainingTimesLastYear: No. of training courses attended last year
 # WorkLifeBalance: 4 Point Likert Scale: 1 'Bad'; 2 'Good'; 3 'Better'; 4 'Best'
 # YearsAtCompany: Time spent with company
 # YearsInCurrentRole: Time in current role
 # YearsSinceLastPromotion: No. of years since last promoted
 # YearsWithCurrManager: Year spent with current manager
 
 setwd("/Users/simoncaton/Documents/OneDriveBusiness/Teaching/ADM/Project and CA/") #change this to where you downloaded the .csv
 hrdata <- read.csv("ADM CA 1 Data.csv", stringsAsFactors = T) #will autoencode the text attributes to factors
 
 #ok, now we need to make a dataset unique to you
 set.seed(1337) # <-- put your student number here WITHOUT the x!! Leave off a starting zero if you have one
 #e.g.: set.seed(62345678)
 my_dataset <- hrdata[order(runif(600)), ]
 
 #let's remove ID, we probably don't want that:
 my_dataset <- my_dataset[-10]
 
 #Unfortunately, due to a technical error, 3 columns of the data were lost :(
 #HR blamed IT, IT blamed HR, your manager will blame you, so let's just hope those columns weren't important!
 col1 <- round(runif(1)*32)+2 #the +2 protects the Age and Attrition Variables
 col2 <- round(runif(1)*31)+2
 col3 <- round(runif(1)*30)+2
 
 cols <- names(my_dataset)
 
 print(paste("I lost: ", cols[col1], ",", cols[col2], ",", cols[col3]))
 #"I lost:  StandardHours , OverTime , Gender"
 
 my_dataset <- my_dataset[-col1]
 my_dataset <- my_dataset[-col2]
 my_dataset <- my_dataset[-col3]
 
 #if you want to use something other than R save your dataset:
 write.csv(file="mydata.csv", my_dataset, row.names = F)
 
 #Now please begin, and good luck!
 #Because you lost 3 columns, some models may/may not work as well, 
 #don't worry about this, I will control for it in the grading!
 
 