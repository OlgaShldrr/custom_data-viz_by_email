# Custom data visualizations by email

This project was developed to send relevant data teasers by email. The script allows sending by email some graphs with data pertained to universities that are similar to the characteristics of the recipient.  The algorithm is placed in a parameterized RMarkdown file that produces a blastula email.

# Decision tree

The algorithm first evaluates whether there are at least six accredited institutions in the dataset, if not it checks if there are six from the same country where chosen university is. If there are less than six universities, the algorithm looks if there are enough universities, and if not, it moves onto a region where the university is located (e.g. South East Asia) and does the same, and if there are not enough universities in that category, then it moves onto macro-regions.

# Used packages

The project is using blastula, RMarkdown and tidyverse.
