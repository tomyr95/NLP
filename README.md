Word prediction is a very useful feature in natural language processing (NLP) applications. It is not only interesting to see, but can actually aid the user by reducing typing fatigue via reduction of keystrokes neeeded. It can also help the user uncover potenaitl areas of further inquiry. It also helps improve spell checking as the user types. A good algorythm is one that is both efficient on resources expended and accuracy of prediction.

<b>PredictWord</b> is an application developed to predict words by implementing one approach to building an algorythm that has a basis the building of an n-gram database to aid with this prediction. An n-gram is a contigous succession of n words taken from a given sample of speech. In our case, we expect n-grams to have meaning, as related to what is the expected correct usage of the English language. Our application makes use of an n-gram, probabilistic language model to achieve this purpose.

The approach is built based on a reference 
<b><a href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip">DataSet</a></b>
 (zip file) of thousands of blogs, news articles, and twitter feeds. This collection of texts or <u>corpus</u> was then split into a training (60%), validation (20%), and testing (20%) datasets. The training dataset was carefully prepared into n=1 to n=5 n-grams by cleaning, tokenization, and further processing of data that was subsequently saved into data tables to be used by the final application.
 
In our app we cleanup and tokenize an n-gram (text input) from the user and apply our algorythm to predict the next word. We will get into more detail in the following slides on how we go about this.

We believe this approach lends itself to ease of implementation based on its portability and simplicity.

The <b>PredictWord</b> application showing our algorythm can be seen 
<b><a href="https://tomyr95.shinyapps.io/WordPredict/">here</a></b>.

<br><br>
Description of Files in Repository:
<br>
<br>FileSubsets.R --- Creates Training, Validation, and Testing files.
<br>CleanTokens2.R --- Cleans-Up Training Data & Crates/Saves N-grams.
<br>CompleteIt.R --- Word Prediction Algorythm.
<br>
<br>benchmark.R --- Runs Common Benchmark from Data Science Capstone.
<br>
<br>5grams_Val_DataPrep.R --- Prepares Data from Validation for Performance Review/Enhancement.
<br>5grams_Val.R --- Validates Model Performance on Validation Data.
<br>
<br>global.R, ui.R, server.R, description.HTLM, instructions.HTML --- Shiny Application Files.
