yawpr
========================================================
author: Bill Kelemen
date: March 25, 2017
autosize: true

yawpr will transform your typing
========================================================

yawpr (yet another word prediction for R) is the ultimate prediction anlgorithm implemented in the R language. 
It has been developed to allow fast real-time word predictions.

To demonstrate the algorithm, a [web application](https://bkelemen.shinyapps.io/pnw_app/) is presented that allows the
user to type words and observe the predictions.

Key features of the algorithm:
- NLP model trained from 1, 2, 3 and 4-grams 
- Incremental model training for large corpus
- Fast prediction of the next word

As we continue to develop the algorithm, we're seeking early investers in the company to fund more advanced prediction algorithms. Please contact [me directly](mailto:bill@kelemen-usa.com?Subject=Interested%20in%20yawpr) for more information.

how does it work?
========================================================
<small>
yawpr is a machine learning algorithm based on [N-gram Language Models and <i>Stupid Backoff</i>] (http://www.aclweb.org/anthology/D07-1090.pdf) that uses a model to preduct the next work. A corpus contains XXX lines and XXX words was provided to develop the algorithm. The model was then trained on a randomly extracted training dataset containing 80% of the corpus, while the testing and validation datasets 10% each. 

The training dataset was cleaned (removing obscene words, special characters, URLs, twitter handles and punctuations). Stop words were not removed as they are important for predictions. This training dataset was then processed by calculating frequencies and sample probabilities as per the <i>Stupid Backoff</i> procedure. Note that alpha (<i>backoff factor</i> is called <i>discount factor</i> in yawpr). 

The model is pruned by only keeping the top five words from each n-gram.

To optimize memory and prediction speed, words are encoded into integers and n-grams 


</small>

how well does it predict?
========================================================

<small>
yawpr has an accuracy prediction between 22-28% against a ramdomly selected testing dataset not used during training.

The training dataset was further divided into smaller datasets so different (smaller) models could be trained. The following table displays the accuracy obtained on models trained on different percentages of training dataset. Calculation was performed on 10% of the test dataset due to computing restrictions):

          model_fname | % training | accuracy  |  size
----------------------|------------|-----------|-------
model-5-0.001-a.cache | 2%         | 0.2211256 | 74 Mb
model-5-0.001-b.cache | 4%         | 0.2325776 | 133 Mb
model-5-0.001-c.cache | 8%         | 0.2428192 | 240 Mb
model-5-0.001-d.cache | 16%        | 0.2555747 | 434 Mb
model-5-0.001-e.cache | 32%        | 0.2699595 | 777 Mb
model-5-0.001-f.cache | 64%        | 0.2825287 | 1.4 Gb
model-5-0.001-g.cache | 100%       | 0.2871375 | 1.6 Gb

</small>

how to test yawpr?
========================================================

<small>
To test yawpr directly, click on this [link](https://bkelemen.shinyapps.io/pnw_app/). The application is self-contained in a multi-page web page that offers, in addition to the prediction algorithm, extensive help and links the the R open source code.
<p>
"Predict words" is where the action is! You can enter text in the provided textbox. As soon as you type a space, the top five predictions are presented. If the prediction was correct, you can select the next word directly. Example:

<img src = "yawpr.png" alt = "yawpr screen shot" width = "500"" height = "300"> 

If you want to tweek the model, you can play around with the "discount factor" (see XXX) and "use unigrams" options. 
</small>
