# Haskell×Haskell - URL shortener λ

## Students group

- Россоха Євген [email](mailto:art6661322@gmail.com)
- Поляков Марк [email](mailto:mark.poliakow@gmail.com)
- Андрющенко Данило [email](mailto:danilandryushenko34@gmail.com)
- Корнійчук Ольга [email](mailto:1704hiolya@gmail.com)

## Design document

The [design document](https://docs.google.com/document/d/16LwsIjEAmsiqBv0mjE9whbA3j2UVXsQz4U74acbpmeA/edit?usp=sharing) that
describes architecture and implementation details of this project.

### System structure

There are several modules:
- `authentication` **authentication module** - creates new users, authenticates existing ones
- `storage` - **database connection module** - a key-value persistence storage
- `urls` - **url-related business logic** - logic of URL shortening
- `api` - **REST API** - a module that provides a REST API and handlers for it
- `models` - **domain types** - declaration of user and link types

## Environment prerequisites

### Haskell
This is a Haskell project, so you will need an environment with installed `GHC` 8.10.3 and `stack` 2.5.1.

### Checkstyle
We use `ormolu` as a code formatter.

## How to start development

1. Clone this repo
2. `$ stack build`

## Commit messages

Write commit messages accordingly by [7 rules of good commit messages](https://chris.beams.io/posts/git-commit/#seven-rules).
