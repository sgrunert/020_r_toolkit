###################################################
#Author: S. Grunert
#Created: 14August2015
#Version: 1.0
#Description:
#This is a Shiny App for Coursera Capstone Project.
###################################################

shinyUI(
  pageWithSidebar(
    # Application Title
    headerPanel(h1("Predicting the Next Word in a Phrase",align = "center")),
    sidebarPanel(width = 5,
      textInput('stringentry', 'Enter a Phrase: ', value = "Example: Try this Shiny prediction application..."),
      submitButton('Submit'),
      h6(strong('Instructions')),
      h6('1) Enter a phrase of several words into the text box above.'),
      h6('2) Click the submit button.'),
      h6('3) View the prediction of a next word to the right.'),
      h6('4) View alternate candidates on the details tab.')
    ),
    mainPanel(width = 7,
              tabsetPanel(
                tabPanel('Prediction',h4('Search words selected for the prediction:'),
                         verbatimTextOutput("inputValue"),
                         h4('The best candidate for the next word in the phrase:'),
                         verbatimTextOutput("prediction"),
                         h4 ('Processing Your Phrase'),
                         h6 ('1) The phrase is stripped of punctuation and other non-alpha characters.'),
                         h6 ('2) The text is standardized to lower case and single spacing between words.'),
                         h6 ('3) Unimportant short words and profanity are removed.'),
                         h6 ('4) The last 3 key words are selected as search words. (NA = Missing a search word.)'),
                         h6 ('5) The search words are used to scan for the most likely next word.')
                         ),
                tabPanel('Detail',h4('List of top next word candidates by relative frequencies: '),
                         tableOutput("others"),
                         h4 ('Selecting the Top Candidate'),
                         h6 ('Candidates are chosen based on the frequencies of trigrams (3-word series) and'),
                         h6 ('unigrams (single words) occurring in a large sample of text from blogs, twitter,'),
                         h6 ('and news feeds. The search words selected from the entered phrase are matched'),
                         h6 ('to the trigrams. The third word of the most frequently matched trigram becomes'),
                         h6 ('a prediction candidate. Ties are resolved using the unigram frequency of the top'),
                         h6 ('candidates. The candidate with the highest unigram frequency wins the tie.')
                         )
              )
    )
  )
)