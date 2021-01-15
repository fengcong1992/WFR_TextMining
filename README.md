# WFR_TextMining
**This is a repository that contains code samples of text mining-based wind forecasting literature review.**

As described in:

**Current status and advances in machine learning-based wind forecasting: A taxonomical review via text mining**


## How to use
The meta data analysis, including the journal infrastructure, author infrastructure, and the publication infrastructure, can be performed directly by the data provided. To scrap the full text data, your own Elsevier API Key is required and can be obtained from https://dev.elsevier.com. 

### Dataset
The example dataset contains meta data scrapped from Google Scholar, Web of Science, and Scopus by the Publish or Perish on 2020-02-01.

### Environment
```
# Install packages within R
install.packages(c("stringr"， “plyr”， “rscopus”， “rvest”， “tm”， “SentimentAnalysis”， “syuzhet”， “tibble”， “tau”， “corpus”))
```


## Publications
**If you use this package in your research, please cite our publications**:

Current status and advances in machine learning-based wind forecasting: A taxonomical review via text mining

**Collaborations are always welcome if more help is needed.**
## License
MIT License, Copyright (c) 2020 Cong Feng

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


## Contact

Cong Feng

joey.fueng@outlook.com