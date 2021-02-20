## code to prepare `my_dataset` dataset goes here

my_dataset <- data.table::fread("C:\\Users\\mai_m\\Desktop\\Loputoo\\Andmed\\ettevotja_rekvisiidid_2021-02-04.csv", encoding="UTF-8")
usethis::use_data(my_dataset, overwrite = TRUE)
