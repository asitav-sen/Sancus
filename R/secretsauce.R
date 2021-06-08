library(shinymanager)
credentials <- data.frame(
  user = c("asitav", "zewdu", "aldo", "rigobert", "merchant1", "merchant3"),
  password = c("lanubia@2021", "lanubia@2021", "lanubia@2021","lanubia@2021", "lanubia@2021", "lanubia@2021"),
  # password will automatically be hashed
  admin = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  company = c("admin","admin","admin","admin","merchant1", "merchant2"),
  premium = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

create_db(
  credentials_data = credentials,
  sqlite_path = "./cred.sqlite", # will be created
  #passphrase = key_get("funmiles", "asitav")
  passphrase = "dreamcatcher"
)
