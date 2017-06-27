require(ascii)
require(xtable)
require(ggplot2)
require(scales)
require(reshape2)
require(plyr)
require(data.table)
require(gridExtra)
require(zoo)

get_total_paid <- function(may_loan_total) {
loan_total <- may_loan_total
monthly_interest_rate <- (0.046 / 12)

total_salary = 30000
threshold = 21000
annual_paid_per_year = total_salary - threshold
paid_from_salary = annual_paid_per_year / 12 * 0.09

may_interest = (loan_total+paid_from_salary) * monthly_interest_rate
may_row = c(0, paid_from_salary, may_interest, max(0, paid_from_salary-may_interest), loan_total)

loan_frame = data.table(v1 = t(may_row))

#loan_frame = data.frame(1, 123, month1interest, 123-month1interest, loan_total-123+month1interest)

names(loan_frame) <- c("Month", "Paid", "Interest", "PrincipalPaid", "TotalLoan")

loan_frame$TotalLoan

last_row <- nrow(loan_frame)
while(loan_frame$TotalLoan[last_row] >= 0)
{
  if (last_row %% 12 == 0)
  {
    total_salary = total_salary * 1.025
    threshold = 21000
    annual_paid_per_year = total_salary - threshold
    paid_from_salary = annual_paid_per_year / 12 * 0.09
  }
  interest_accured = (loan_frame$TotalLoan[last_row]-paid_from_salary) * monthly_interest_rate
  principal_paid = max(0, paid_from_salary-interest_accured)
  loan_frame = rbind(loan_frame, list(loan_frame$Month[last_row]+1, 
                         paid_from_salary, 
                         interest_accured,
                         principal_paid,
                         loan_frame$TotalLoan[last_row]-paid_from_salary+interest_accured))
  last_row <- nrow(loan_frame)
}
loan_frame[nrow(loan_frame)]$Paid = loan_frame[nrow(loan_frame)-1]$TotalLoan
loan_frame[nrow(loan_frame)]$Interest = 0
loan_frame[nrow(loan_frame)]$PrincipalPaid = loan_frame[nrow(loan_frame)-1]$TotalLoan
loan_frame[nrow(loan_frame)]$TotalLoan = 0

#loan_frame <- loan_frame[25:nrow(loan_frame),]

interest_frame = cbind(loan_frame[, c("Month", "Interest")], Account="Interest")
colnames(interest_frame)[2] <- "Amount"
principal_paid_frame = cbind(loan_frame[, c("Month", "PrincipalPaid")], Account="PrincipalPaid")
colnames(principal_paid_frame)[2] <- "Amount"
total_loan_frame = cbind(loan_frame[, c("Month", "TotalLoan")], Account="TotalLoan")
colnames(total_loan_frame)[2] <- "Amount"

tableable_frame <- rbind(interest_frame, principal_paid_frame)
tableable_frame <- rbind(tableable_frame, total_loan_frame)
tableable_frame <- cbind(tableable_frame, Year=(tableable_frame$Month / 12)+1)
#tableable_frame <- cbind(tableable_frame, Year=(tableable_frame$Month %/% 12)+1)
#summed_frame <- tableable_frame[, sum(Amount), by = c("Year", "Account")]

total_plot = ggplot(tableable_frame, aes(x=Year, y=Amount, fill=Account))+geom_area(position = "stack")+
  geom_hline(yintercept = paid_from_salary) + 
  annotate("text", x=mean(tableable_frame$Year), y=-200, label = "Monthly Loan Payment") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_x_continuous(breaks = 1:(ceiling(max(tableable_frame$Year))+1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix="£"))

principal_vs_interest_plot = ggplot(subset(tableable_frame, Account %in% c("Interest", "PrincipalPaid")), aes(x=Year, y=Amount, fill=Account))+geom_area(position = "stack")+geom_hline(yintercept = paid_from_salary) + 
  annotate("text", x=mean(tableable_frame$Year), y=paid_from_salary+10, label = "Monthly Loan Payment") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_x_continuous(breaks = 1:(ceiling(max(tableable_frame$Year))+1)) +
  scale_y_continuous(labels = scales::dollar_format(prefix="£"))

grid.arrange(total_plot, principal_vs_interest_plot, ncol=2)

sumOfInterest = sum(loan_frame$Interest)
sumOfInterest
sumOfInterestAsPercentOfLoan = sum(loan_frame$Interest)/loan_total
percent(sumOfInterestAsPercentOfLoan)

#Some advance analysis of the idea of paying off part of the loan.
rollapply(loan_frame$Interest, width = 12, by = 12, FUN = sum, align = "left") / (paid_from_salary * 12)
rollapply(loan_frame$Interest, width = 12, by = 12, FUN = sum, align = "left") / (sum(loan_frame$Interest))
return(c(sum(loan_frame$Paid[0:(30*12)], na.rm = TRUE), min(30, ceiling(nrow(loan_frame) / 12))))
}
#30000 -> 61837.3
#total_loan = 21482
total_loan = 30000
early_payment = cbind(0, t(get_total_paid(total_loan)), 0, 0, 0)
for (i in seq(total_loan-1000, 0, by=-1000))
{
  upfront_payment = total_loan-i
  result = t(get_total_paid(i))
  saving = early_payment[1, 2]-result[1]-upfront_payment
  roi = saving/upfront_payment
  roi_years = early_payment[1, 3]+1
  early_payment = rbind(early_payment, 
                              cbind(upfront_payment, result, 
                                    saving, 
                                    roi,
                                    roi/roi_years))
}
early_payment_frame = data.frame(early_payment)
names(early_payment_frame) = c("UpfrontPaid", "TotalPaidFromSalary", "TotalYearsPaid", "Savings", "ROI", "ROIPerYear")

ggplot(early_payment_frame, aes(x=UpfrontPaid, y=Savings))+geom_point()+
     scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
     scale_y_continuous(labels = scales::dollar_format(prefix="£"))

ggplot(early_payment_frame, aes(x=UpfrontPaid, y=ROIPerYear))+geom_point()+
     scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
     scale_y_continuous(labels = scales::percent)
