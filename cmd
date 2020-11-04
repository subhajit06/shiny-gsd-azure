sudo docker build -t subhajit006/shiny-gsd:v1 .

sudo docker run --rm -p 29579:29579 -v /home/azureuser/test_shiny_gsDesign/json:/usr/local/src/app/json subhajit006/shiny-gsd:v1

sudo docker login
sudo docker push subhajit006/shiny-gsd:v1
