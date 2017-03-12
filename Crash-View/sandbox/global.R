if (!require(devtools)) install.packages("devtools")

devtools::install_github(repo="skranz/restorepoint")
devtools::install_github(repo="skranz/shinyEvents")


library(shinyEvents)
library(shinyAce)

app = eventsApp()

# app$glob can contain "global" variables that are visible
# for all sessions.
# app$glob$txt will be the content of the chat window
app$glob$txt = "Conversation so far"

app$ui = fluidPage(
  textInput("userName","User Name",""),
  
  # Chat window
  aceEditor("convAce",value = app$glob$txt, height="200px",
            showLineNumbers = TRUE, debounce=100),    
  
  # Enter new text
  aceEditor("enterAce",value = "Your text",height="30px",
            showLineNumbers = TRUE,debounce = 100,
            hotkeys = list(addTextKey="Ctrl-Enter")),
  
  actionButton("addBtn", "add")
)

addChatText = function(session,app,...) {
  restore.point("addChatText")
  user = getInputValue("userName")
  str = getInputValue("enterAce")
  app$glob$txt = paste0(app$glob$txt,"\n",user, ": ",paste0(str,collapse="\n"))
  updateAceEditor(session,"convAce", value = app$glob$txt)
  updateAceEditor(session,"enterAce", value = " ")
}

# Add chat text when button or Ctrl-Enter is pressed 
buttonHandler(id="addBtn",addChatText)
aceHotkeyHandler("addTextKey",addChatText)

# refresh chat window each second
timerHandler("refreshChatWindow",1000, function(session,app,...) {
  txt = getInputValue("convAce")
  if (!identical(txt, app$glob$txt)) {
    cat("Refresh chat window...")
    updateAceEditor(session, "convAce", value = app$glob$txt)
  }
})


# Initialize each new session with a random user name
appInitHandler(function(input, output, session,app,...) {
  updateTextInput(session,"userName",
                  value=paste0("guest", sample.int(10000,1)) )
  updateAceEditor(session,editorId = "convAce",value = app$glob$txt)
})


runEventsApp(app, launch.browser=TRUE)
# To test chat function, open several browser tabs