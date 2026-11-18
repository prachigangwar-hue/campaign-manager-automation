# Function to send simple text messages to Telegram


SEND_TO_TELEGRAM <- function(chat_id, message, type = "text"){
  require(httr)
  bot_token = "REDACTED_REQUIRED"
  
  if(type == "text"){
    lapply(chat_id,
           function(x){
             generate_api_text <- paste0('https://api.telegram.org/bot', bot_token , '/sendMessage?chat_id=' , x , '&parse_mode=Markdown&text=' , message)
             result <- GET(generate_api_text)
             print(result)
           })
  }
  if(type == "image"){
    
  }
  if(type == "video"){
    
  }
}


# import requests
# 
# 
# def telegram_bot_sendtext(bot_message):
#   
# bot_token = 'REDACTED_REQUIRED'
# bot_chatID = '745474876'
# send_text = 'https://api.telegram.org/bot' + bot_token + '/sendMessage?chat_id=' + bot_chatID + '&parse_mode=Markdown&text=' + bot_message
# 
# response = requests.get(send_text)
# 
# return response.json()
# 
# 
# test = telegram_bot_sendtext("Testing Telegram bot")
# print(test)
# 

SEND_GMAIL_EMAIL <- function(path_cred, path_cache, path_attach, to_emails,cc_emails = NULL, check_draft = FALSE, message_sub, message_text){
  
  # Configure your app
  gm_auth_configure(path = path_cred)
  
  # Authenticate with the new cache, store tokens in .secret
  gm_auth(email = TRUE,cache = path_cache)
  #browser()
  # Creating an email
  test_email <-
    gm_mime() %>%
    gm_to(to_emails) %>%
    gm_cc(cc_emails) %>% 
    gm_subject(message_sub) %>%
    gm_html_body(message_text)
  
  # Lopping over attachments
  if(!is.null(path_attach)){
    for(a in path_attach){
      test_email <- test_email %>% gm_attach_file(a)
    }
  }
  
  # Sending email
  if(check_draft == TRUE){
    gm_create_draft(test_email)
    
  }
  
  gm_send_message(test_email)
  
}

