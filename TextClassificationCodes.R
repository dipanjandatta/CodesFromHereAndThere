#Clear the memory file
rm(list=ls())

#Set working directory
setwd("C:\\Users\\dipanjand\\Desktop\\Imacs")

# Get a list of files 
Amtekfilelist = list.files(pattern = "Amtek.*.txt")
Boschfilelist = list.files(pattern = "Bosch.*.txt")
Testfilelist = list.files(pattern = ".*.txt")

## Read data
# For Amtek Auto

Amtek = character(length(Amtekfilelist))

for (i in 1:length(Amtekfilelist)) {
        Amtek[i] = paste(readLines(Amtekfilelist[i]), collapse=" ")
}

# For Bosch
Bosch = character(Boschfilelist)

for (i in 1:Boschfilelist) {
        Bosch[i] = paste(readLines(Boschfilelist[i]), collapse=" ")
}

# For testing, both Amtek and Bosch
Test = character(Testfilelist)

for (i in 1:Testfilelist) {
        Test[i] = paste(readLines(Testfilelist[i]), collapse=" ")
}

# Load required packages

library(tm)

DocumentCleaning = function(docVector){
        
        # Inputs doc vector, output cleaned corpus
        docs = Corpus(VectorSource(docVector))
        
        docs = tm_map(docs, stripWhitespace)
        
        docs = tm_map(docs, removeWords, stopwords("english"))
        
        docs = tm_map(docs, removePunctuation)
        
        docs = tm_map(docs, removeNumbers)
        
        docs = tm_map(docs, tolower)
        
        docs = tm_map(docs, PlainTextDocument)
        
        docs
                
}

Amtek.corpus = DocumentCleaning(Amtek)
Bosch.corpus = DocumentCleaning(Bosch)
Test.corpus = DocumentCleaning(Test)

Amtek.matrix = t(TermDocumentMatrix(Amtek.corpus,
                                    control = list(wordLengths=c(4,Inf))))
Bosch.matrix = t(TermDocumentMatrix(Bosch.corpus,
                                    control = list(wordLengths=c(4,Inf))))

Test.matrix = t(TermDocumentMatrix(Test.corpus,
                                   control = list(wordLengths=c(4,Inf))))

probabilityMatrix <-function(docMatrix)
{
        # Sum up the term frequencies
        termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
        # Add one
        termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
        # Calculate the probabilties
        termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
        # Calculate the natural log of the probabilities
        termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
        # Add pretty names to the columns
        colnames(termSums)<-c("term","count","additive","probability","lnProbability")
        termSums
}

Amtek.pMatrix = probabilityMatrix(Amtek.matrix)
Bosch.pMatrix = probabilityMatrix(Bosch.matrix)

getProbability <- function(testChars,probabilityMatrix)
{
        charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
        # Count how many words were not found in the mandrill matrix
        charactersNotFound<-length(testChars)-length(charactersFound)
        # Add the normalized probabilities for the words founds together
        charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
        # We use ln(1/total smoothed words) for words not found
        charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
        #This is our probability
        prob<-charactersFoundSum+charactersNotFoundSum 
        prob
}

# Get the matrix
Test.matrix = as.matrix(Test.matrix)
# A holder for classification 
classified = NULL

AmtekScore = NULL

BoschScore = NULL

for(j in 1:nrow(Test.matrix))
{
        # Extract the test words
        Test.chars = names(Test.matrix[j,Test.matrix[j,] %in% 1])
        # Get the probabilities
        AmtekScore[j] = getProbability(Test.chars,Amtek.pMatrix)
        BoschScore[j] = getProbability(Test.chars,Bosch.pMatrix)
        # Add it to the classification list
        classified = c(classified,
                       ifelse(AmtekScore[j]>BoschScore[j],"Risky","Non-Risky"))
}

Final_Out = data.frame(Test,classified,
                       "Risk_Score"= round(10-log(-AmtekScore),2),
                       "Safe_Score"= round(10-log(-BoschScore),2)
)

View(Final_Out)


