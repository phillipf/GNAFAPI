
Sys.setenv(JAVA_HOME='C:\\Program Files\\ojdkbuild\\java-1.8.0-openjdk-1.8.0.111-3\\jre\\')
install.packages("rJava")
install.packages("RH2")

library(RH2)

con <- dbConnect( JDBC('org.h2.Driver', 'C:/Users/farrelp1/.ivy2/cache/com.h2database/h2/jars/h2-1.4.193.jar') , 'jdbc:h2:~/gnaf', 'sa', '')

a <- dbGetQuery(con, "SELECT * FROM ADDRESS_VIEW LIMIT 1")
