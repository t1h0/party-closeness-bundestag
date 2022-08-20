# Party closeness in the Bundestag

How closely to their party's manifesto do german politicians speak in the Bundestag?

## Research background

The main script (__party-closeness-bundestag.R__) investigates the relation between politicians' speeches in the Bundestag and their party's manifesto by looking at TF-IDF scores.

## What comes with it?

* __bundestag_process.R__ provides useful functions for scraping Bundestag protocols of the current term and also for extracting actual speech text per parliamentarian out of any set of given (or scraped) Bundestag protocols. Lastly, functions for creating TF-IDF document-term-matrices out of the given speech corpus are also included.
* __manifestos_process.R__ provides useful functions for downloading parties manifestos (by use of the [Manifesto Project](https://manifesto-project.wzb.eu/)'s data through [ManifestoR](https://github.com/ManifestoProject/manifestoR/)) and creating TF-IDF document-term-matrices out of them.
* __analysis.R__ provides a rather rudimentary analysis of the relation between speeches and manifestos. This was mainly research driven and created for the initial academical submission and is therefore not (yet) well structured and hardly reusable.

## Thanks to

The [Manifesto Project](https://manifesto-project.wzb.eu/) and [ManifestoR](https://github.com/ManifestoProject/manifestoR/).

## Disclaimer

The initial commit was my final submission to an r course during my master's studies.
