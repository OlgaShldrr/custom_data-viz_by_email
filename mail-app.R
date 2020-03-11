library(blastula)
library(readr)

emails <- read_csv("emails.csv")


send_email <- function(school, email) {
  email_object <- blastula::render_email("test.Rmd", render_options = list(params = list(id=school)))
  
  #email_object <- prepare_test_message()
  
  email_object %>% 
    blastula::smtp_send(
    from = "XXX@XXX.com",
    to = email,
    subject = "Here are some data insights",
    credentials = creds_file(".email_creds")
    
  )
}



send_emails <- function(cleaner_dataset) {
  
  email_list <- as.list(cleaner_dataset)
  
  purrr::possibly(
    purrr::walk2(
      .x = email_list$school,
      .y = email_list$email,
      .f = ~ send_email(school = .x, email = .y)
    ), 
    otherwise = "The email message was not sent successfully"
  )
}

send_emails(emails)
