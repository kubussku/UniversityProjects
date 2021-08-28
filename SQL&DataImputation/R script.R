# Database file dir - 'F:/SQL_TID Projekt/sqlite-sakila.db'
# Resources: 
# https://db.rstudio.com/

# Libs
library(odbc)
library(DBI)
library("RSQLite")

# Connect to database from file
con <- dbConnect(RSQLite::SQLite(), 'F:/SQL_TID Projekt/sqlite-sakila.db')
# List all tables
dbListTables(con)
# Print table head for each table in database
for (i in (dbListTables(con))){
  print.data.frame(head(dbReadTable(con, i)))
}
# Standardowe wysłanie query przez syntax SQL'a
res <- dbSendQuery(con, "SELECT * FROM actor")
dbFetch(res)
# Zapytania sql przez funkcje opisane w pakiecie DBI
dbReadTable(con, "actor") # %>% mozna uzywac pipe operatorów z r-a

# Zapytania sql przez biblioteke dplyr, cześć bibliotek z tidyverse sluzacych do zarzadzania danymi w R
library(dbplyr)
library(dplyr)
film = tbl(con, "film")
film %>%
  summarise(rental_rate = mean(rental_rate))

## __ TID
# Imputacje danych na bazie pingwinów :)
# https://www.r-bloggers.com/2021/01/generating-sql-with-dbplyr-and-sqlfluff/
    
# Jacy(ranking) aktor zagral w najwiekszej ilosci różnych filmów
# Jakas tablica statystyk na bazie `film`, np. z relacją z film category_ Tablica czestosci kategorii filmów
# Statystyki dotyczace Jezyka filmów 
# Statystyki opisowe wylacznie na podstawie tablicy `film`


# Statystyki dotyczace tablicy `customer` - np. rozklady. czestosc imion. Aktywnosc. 
#   /Relacyjne np. kto jak placil. z jakich miast sa nasi customerzy, , z jakich krajów
#   / Kto ile placi. ile razy.

