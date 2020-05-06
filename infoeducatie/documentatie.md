# Documentatie

## Table of contents

## Tehnologii

Proiectul a fost scis in [purescript](https://www.purescript.org/), un limbaj de programare pur functionala cu sintaxa aproape indentica cu haskell dar care poate fi compilat in javascript.

- Pentru rendare web am folosit [halogen](https://github.com/purescript-halogen/purescript-halogen), o librarie care extinde arhitectura elm cu suport pentru mai mult de 1 component, alegerea orcarui monad pentru rularea interfatei si multe altele.
- Pentru pacakge managementul dependentelor de pe [npm](https://www.npmjs.com/) am folosit [pnpm](https://pnpm.js.org/) - un cli care rezolva multe din problemele actuale aflate in npm, iar pentru (package managementul) dependentelor pentru purescript am folosit [spago](https://github.com/purescript/spago).
- Pentru bundlingul aplicatie am folosit [parcel](https://parceljs.org/) iar pentru stiluri am folosit [scss](https://sass-lang.com/).
- Pentru inferarea tipurilor am implementat [algoritmul Hindley Milner de inferare a tipurilor](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) impreuna cu un sistem care transforma programele vizuale intr-o expresie lineara.
- Proiectul foloseste [hygen](https://www.hygen.io/) pentru creerea de templateuri ce pot fi refolosite.
- Pentru a recompila partea de purescript si back-endul la fiecare schimbare am folosit [nodemon](https://www.hygen.io/).
- Proiectul respecta standardul [all-contributors](https://github.com/all-contributors/all-contributors)
- Pentru CI & CD am folosit [semantic-release](https://semantic-release.gitbook.io/semantic-release/) impreuna cu [github-actions](https://github.com/features/actions)

## Arhitectura

TODO

## Portabilitate

Aplicatia ruleaza pe orice pc ce poate rula o versiue recenta de chrome sau firefox.

## Testarea

Proiectul se bazeaza cel mai mult pe user-testing, dar am scris si cateva unit testuri pentru a ma asigura ca algoritmul de aranjare a arcurilor pe mai multe cercuri functioneaza cu mult inainte de a implementa partea grafica.

## Gestionarea codului

De la inceput am folosit [git](). Deoarece in fiecare repo (front-end / back-end) a lucrat o singura persoana am folosit un sistem de branching destul de simplu:

Branchul default este `develop` care contine cele mai recente surse ale aplicatii. Fiecare commit in develop este testat si construit automat folosind github actions. Mereu cand vreau sa fac un release creeez un pull request in `master`. Dupa ce totul e testat si compilat se genereaza un changelog din commituri (acest lucru este posibil deoarece folosesc [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/)) si un github release avand ca descriere sectiunea din changelog a respectibului release. Dupa acesti pasi proiectul este publicat automat pe [netlify](https://www.netlify.com/).

## Structura de fisiere

Aici este o rendare a structurii de fisiere generata folosind [gource](https://gource.io/)

![file structure](./assets/file-structure.png)
