yawpr
========================================================
author: Bill Kelemen
date: March 25, 2017
autosize: true
css: custom.css

yawpr will transform your typing
========================================================

<small>
<b>yawpr</b> (yet another word predictor for R) is the ultimate prediction algorithm implemented in the R language. 
It was developed to allow fast real-time word predictions within a simple user interface.

To demonstrate the algorithm, a [web application](https://bkelemen.shinyapps.io/pnw_app/) is presented that allows the
user to type words and observe the predictions.

Key features of the algorithm:
- N-gram Language Model trained from 1, 2, 3 and 4-grams 
- Incremental model training for large corpus
- Fast prediction of the next word

As we continue to develop the algorithm, we're seeking early investors in the company to fund more advanced prediction algorithms. Please contact [me directly](mailto:bill@kelemen-usa.com?Subject=Interested%20in%20yawpr) for more information.
</small>

how does it work?
========================================================

<small>
<b>yawpr</b> is a machine learning algorithm based on a modified version from the [N-gram Language Models and <i>Stupid Backoff</i>] (http://www.aclweb.org/anthology/D07-1090.pdf) paper. The model is trained on 1, 2, 3 and 4-gram's, where each n-gram (w[n] w[n-1] ... w[2] w[1], w[i] = word) is divided into the <i>root</i> (w[n] w[n-1] ... w[2]) and the last <i>word</i> (w[1]). Sample probabilities are then computed for all n-grams: p(<i>word</i>|<i>root</i>) = count(<i>word</i>|<i>root</i>) / count(<i>word</i>).

The model is trained on 80% from a corpus of 4 million lines and 100 million words, while the remaining 20% is reserved for testing and validation datasets (10% each). 

The training dataset is cleaned (removing obscene words, special characters, URLs, twitter handles and punctuation) but other stop words are not removed (so they can be predicted). This training dataset is then processed by computing frequencies and sample probabilities as explained above.

To predict the next word, a search is initiated at the highest n-gram model depending on the <i>root</i> already seen. The highest probability <i>words</i> following the <i>root</i> matching the n-1 words already seen are selected. In doing so, a new probability is calculated by discounting the word count by a <i>discount_factor</i> (parameter to the algorithm). The algorithm recursively moves to the lower order n-gram (by removing the fist word from the <i>root</i>) and repeating the search. A smoothing parameter is applied for lower level probabilities. As we descend the n-gram levels, the search is optimized by only selecting words that could improve the prediction and have not been predicted yet.

Finally, optimization in memory utilization and speed are implemented by using the `data.table` `R` package (with indices), encoding words as integers and hashing n-grams roots.
</small>

how well does it predict?
========================================================

<small>
<b>yawpr</b> has an accuracy prediction between 22-28% when tested against a randomly selected dataset not used during training.

Multiple models were created with varying sized datasets from the overall training dataset. These models were then validated against a section of the test dataset (due to computing restrictions). The following table displays the accuracies obtained on each:

          model_fname | % of training | accuracy  |  size  | comment
----------------------|---------------|-----------|--------|-------------------------
model-5-0.001-a.cache | 2%            | 0.2211256 | 74 Mb  |
model-5-0.001-b.cache | 4%            | 0.2325776 | 133 Mb |
model-5-0.001-c.cache | 8%            | 0.2428192 | 240 Mb | used in the shiny app
model-5-0.001-d.cache | 16%           | 0.2555747 | 434 Mb |
model-5-0.001-e.cache | 32%           | 0.2699595 | 777 Mb |
model-5-0.001-f.cache | 64%           | 0.2825287 | 1.4 Gb |
model-5-0.001-g.cache | 100%          | 0.2871375 | 1.6 Gb |

This model can be further improved by using more advanced NLP algorithms. Time did not allow to explore these during this project.
</small>

can't wait...how do you test yawpr?
========================================================

<small>
To test <b>yawpr</b> directly, click on this [link](https://bkelemen.shinyapps.io/pnw_app/). The application is self-contained in a multi-page web site that offers, in addition to the prediction algorithm, extensive help and links to the the R open source code.
<p>
"Predict words" menu item is where the action is! You can enter text in the provided textbox. As soon as you type a space, the top five predictions are presented. If the prediction was correct, you can select the next word directly by clicking on it. Example:

<img src = "yawpr.png" alt = "yawpr screen shot" width = "550"" height = "350"> 

If you want to tweak the model, you can play around with the <i>discount_factor</i> as well as "use unigrams in backoff" options. 

Enjoy!
</small>
