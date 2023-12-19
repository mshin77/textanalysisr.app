Dockerfile on GitHub: 

Repo name: mshin77/textanalysisr
docker build -t mshin77/textanalysisr .
docker tag  mshin77/textanalysisr  mshin77/textanalysisr
docker push  mshin77/textanalysisr
docker run -p 3838:3838 mshin77/textanalysisr