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

# Curl usage
## Sign up
```sh
$ curl -v -X POST "localhost:3001/users/signup?email=petuch&password=admin"
*   Trying ::1:3001...
* connect to ::1 port 3001 failed: Connection refused
*   Trying 127.0.0.1:3001...
* Connected to localhost (127.0.0.1) port 3001 (#0)
> POST /users/signup?email=petuch&password=admin HTTP/1.1
> Host: localhost:3001
> User-Agent: curl/7.75.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Fri, 19 Mar 2021 19:13:52 GMT
< Server: Warp/3.3.13
< Content-Type: application/json;charset=utf-8
<
* Connection #0 to host localhost left intact
```
## Sign in
```sh
$ curl -v -X POST "localhost:3001/users/signin?email=petuch&password=admin"
*   Trying ::1:3001...
* connect to ::1 port 3001 failed: Connection refused
*   Trying 127.0.0.1:3001...
* Connected to localhost (127.0.0.1) port 3001 (#0)
> POST /users/signin?email=petuch&password=admin HTTP/1.1
> Host: localhost:3001
> User-Agent: curl/7.75.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 204 No Content
< Date: Fri, 19 Mar 2021 19:15:17 GMT
< Server: Warp/3.3.13
< Content-Type: application/json;charset=utf-8
< Set-Cookie: JWT-Cookie=eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsidXNlckhhc2giOiJhZG1pbiIsInVzZXJFbWFpbCI6InBldHVjaCJ9fQ.llKZqV1bouOJRq1p_OWqw6Jz6vezjnZlXedN3Xm3vVdAHUdbOMtsUNgNJZcxVF66nJvEc9K5qK-JWEomqLXdSA; Path=/; HttpOnly; Secure; SameSite=Lax
< Set-Cookie: XSRF-TOKEN=fYEqPcQ/FsCp+u+s81aQqfv5P7g5zoGa0yw8BwhcUHI=; Path=/; Secure
<
* Connection #0 to host localhost left intact
```
## Shorten
```sh
$ curl -v -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsidXNlckhhc2giOiJhZG1pbiIsInVzZXJFbWFpbCI6InBldHVjaCJ9fQ.llKZqV1bouOJRq1p_OWqw6Jz6vezjnZlXedN3Xm3vVdAHUdbOMtsUNgNJZcxVF66nJvEc9K5qK-JWEomqLXdSA" -X POST "localhost:3001/urls/shorten?url=https://nyaa.si"
*   Trying ::1:3001...
* connect to ::1 port 3001 failed: Connection refused
*   Trying 127.0.0.1:3001...
* Connected to localhost (127.0.0.1) port 3001 (#0)
> POST /urls/shorten?url=https://nyaa.si HTTP/1.1
> Host: localhost:3001
> User-Agent: curl/7.75.0
> Accept: */*
> Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsidXNlckhhc2giOiJhZG1pbiIsInVzZXJFbWFpbCI6InBldHVjaCJ9fQ.llKZqV1bouOJRq1p_OWqw6Jz6vezjnZlXedN3Xm3vVdAHUdbOMtsUNgNJZcxVF66nJvEc9K5qK-JWEomqLXdSA
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Fri, 19 Mar 2021 19:18:27 GMT
< Server: Warp/3.3.13
< Content-Type: application/json;charset=utf-8
< Set-Cookie: XSRF-TOKEN=x/jlJLqv9bp7oMrm0hqTQFcL6DXf9TSfObrun3TEaHo=; Path=/; Secure
< Set-Cookie: JWT-Cookie=eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsidXNlckhhc2giOiJhZG1pbiIsInVzZXJFbWFpbCI6InBldHVjaCJ9fQ.llKZqV1bouOJRq1p_OWqw6Jz6vezjnZlXedN3Xm3vVdAHUdbOMtsUNgNJZcxVF66nJvEc9K5qK-JWEomqLXdSA; Path=/; HttpOnly; Secure; SameSite=Lax
<
* Connection #0 to host localhost left intact
"rtjhvbc"
```
## Redirect
```sh
$ curl -v -X GET "localhost:3001/r/rtjhvbc"
Note: Unnecessary use of -X or --request, GET is already inferred.
*   Trying ::1:3001...
* connect to ::1 port 3001 failed: Connection refused
*   Trying 127.0.0.1:3001...
* Connected to localhost (127.0.0.1) port 3001 (#0)
> GET /r/rtjhvbc HTTP/1.1
> Host: localhost:3001
> User-Agent: curl/7.75.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 303 See Other
< Transfer-Encoding: chunked
< Date: Fri, 19 Mar 2021 19:19:24 GMT
< Server: Warp/3.3.13
< Content-Type: text/plain;charset=utf-8
< Location: https://nyaa.si
<
* Connection #0 to host localhost left intact
```
