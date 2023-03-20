library("dplyr")

library("gtools")

library("arules")

rawData <- read.csv("grc.csv" , stringsAsFactors = FALSE)


barplot(
  height = table(rawData$paymentType) ,
  col = "skyblue",
  main = "Compare between cash & credit",
  xlab = "operation",
  ylab = "count"
  
)


pie(x = table(rawData$paymentType),
    main = "Compare payment by count")







get_total <- function(list) {
  total = 0
  if (length(list) != 0) {
    for (index in 1:length(list)) {
      total = total + list[index]
      
      
    }
    
    return(total)
  } else
    return(0)
  
  
  
}



age_1_to_10 = filter(rawData, (age <= 10 & age > 0))

list_1_to_10 = as.vector(age_1_to_10$total)

total_1_to_10 = get_total(list_1_to_10)




age_11_to_20 = filter(rawData, (age <= 20 & age > 10))

list_11_to_20 = as.vector(age_11_to_20$total)

total_11_to_20 = get_total(list_11_to_20)



age_21_to_30 = filter(rawData, (age <= 30 & age > 20))

list_21_to_30 = as.vector(age_21_to_30$total)

total_21_to_30 = get_total(list_21_to_30)



age_31_to_40 = filter(rawData, (age <= 40 & age > 0))

list_31_to_40 = as.vector(age_31_to_40$total)

total_31_to_40 = get_total(list_31_to_40)



age_41_to_up = filter(rawData, (age > 40))

list_41_to_up = as.vector(age_41_to_up$total)

total_41_to_up = get_total(list_41_to_up)


compare_age_spending <- data.frame(
  age = c("1 to 10", "11 to 20"  , "21 to 30" , "31 to 40" , "40  to up") ,
  total = c(
    total_1_to_10 ,
    total_11_to_20  ,
    total_21_to_30 ,
    total_31_to_40 ,
    total_41_to_up
  )
  
  
)

barplot(
  height = compare_age_spending$total,
  name = compare_age_spending$age,
  col = "skyblue",
  main = "Compare between age spending",
  xlab = "age",
  ylab = "total"
)


plot(
  x = rawData$age,
  y = rawData$total,
  main = "total vs. Age",
  xlab = "Age",
  ylab = "total"
)






cites_list = group_by(rawData , city)
cites_list = summarise(cites_list , total = sum(total))
cites_frame = as.data.frame(cites_list)
cites_frame <- arrange(cites_frame , desc(total))


barplot(
  height = cites_frame$total,
  name = cites_frame$city,
  col = "skyblue",
  main = "Compare between cities spending",
  xlab = "city",
  ylab = "total"
)


boxplot(x = cites_frame$total ,
        main = "Distribution cities spending" ,
        xlab = "spending")










customer_list = group_by(rawData , customer , age)
customer_list = summarise(customer_list , totalsal = sum(total))
final_data_matrix = cbind(customer_list$age , customer_list$totalsal)
row.names(final_data_matrix) <- customer_list$customer
cin <- as.numeric(readline("Enter the number of cintroids : "))

result <- kmeans(final_data_matrix, centers = cin)
result





items <- rawData$items
items
write.csv(items , "items.txt"  , quote = FALSE ,   row.names = F)


transactions <- read.transactions("items.txt" , sep = ",")
class(transactions)
inspect(transactions)


min_support <- as.double(readline("Enter the min support : "))
min_confidance <- as.double(readline("Enter the min confidance : "))


apriori_rules = apriori(transactions ,
                        parameter = list(
                          supp  = min_support ,
                          conf = min_confidance ,
                          minlen = 2
                        ))
inspect(apriori_rules)
