## Requirements

- `textdata` package
- Internet connection (first download only)

## Installation

```r
install.packages("textdata")

library(tidytext)
get_sentiments("bing")     # Positive/negative
get_sentiments("afinn")    # Scores -5 to +5
get_sentiments("nrc")      # Emotions + sentiment
```

Lexicons are cached locally after first download.

## Available Lexicons

- **Bing**: Binary positive/negative classification
- **AFINN**: Numeric sentiment intensity (-5 to +5)
- **NRC**: Multi-label with 8 emotions + sentiment

## Features Enabled

- Sentiment analysis with multiple lexicons
- Emotion detection (joy, anger, fear, etc.)
- Document-level sentiment scoring
