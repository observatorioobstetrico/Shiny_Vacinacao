import requests
import json
import time
import http.server
import socketserver
import csv
from apscheduler.schedulers.background import BackgroundScheduler
from datetime import datetime

def get_dados_vacinacao(grupoAtendimento = '001801'):
  start_time = time.time()
  print(f'Iniciando exportação de registros do grupo de atendimento {grupoAtendimento} as {datetime.fromtimestamp(start_time).strftime("%H:%M:%S")}')
  total = 0
  response = requests.post("https://imunizacao-es.saude.gov.br/_search?scroll=1m", # scroll=1m - Gerar um _scroll_id (token) para paginação com retenção do histórico de paginação de 1 mês
                           json={ 
                              'size': 10000, # máximo de registros por consulta
                              'query': {
                                 'bool': {
                                    'filter': [{
                                       'term': { 
                                          'vacina_grupoAtendimento_codigo': grupoAtendimento # filtro grupo de atendimento gestantes e puérperas
                                        }
                                      }]
                                    }
                                  }
                              }, 
                           auth=('imunizacao_public', 'qlto5t&7r_@+#Tlstigi'), 
                           headers={'Content-type': 'application/json'})
  json_response = json.loads(response.text)
  print(f'Total de registros a serem exportados: {json_response["hits"]["total"]["value"]}')
  hits = [ dict(sorted(hit['_source'].items())) for hit in json_response['hits']['hits'] ]
  csv_header = hits[0].keys()

  with open(f'vacinacao_covid_{grupoAtendimento}.csv', 'w', encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=csv_header)
    writer.writeheader()
    writer.writerows(hits)

  total += len(hits)
  print(f'Registros exportados: {total}')

  while (hits):
    response = requests.post("https://imunizacao-es.saude.gov.br/_search/scroll", # /scroll - indica que vai consultar as próximas páginas da consulta
                           json={ 'scroll': '1m', 'scroll_id': json.loads(response.text)['_scroll_id'] }, # scroll_id - token _scroll_id gerado na requisição anterior, indica que deve trazer a página seguinte a requisição anterior. 
                           auth=('imunizacao_public', 'qlto5t&7r_@+#Tlstigi'), 
                           headers={'Content-type': 'application/json'})
    hits = [ dict(sorted(hit['_source'].items())) for hit in json.loads(response.text)['hits']['hits'] ]
    with open(f'vacinacao_covid_{grupoAtendimento}.csv', 'a', encoding="utf-8") as f:
      writer = csv.DictWriter(f, fieldnames=csv_header)
      writer.writerows(hits)

    total += len(hits)
    print(f'Registros exportados: {total}')
    
  print(f'Finalizando exportação de registros do grupo de atendimento {grupoAtendimento} as {time.strftime("%H:%M:%S")}')
  print(f'Importado {total} de registros em {"{:,.2f}".format((time.time() - start_time)/60)} minutos')
  return hits

get_dados_vacinacao('001901')
get_dados_vacinacao('001801')






# # apscheduler - pacote para agendar execução da consulta
# scheduler = BackgroundScheduler()
# scheduler.start()
# # agendando a consulta de dados de vacinação para executar toda segunda as 12:30
# scheduler.add_job(get_dados_vacinacao, 'cron', day_of_week='mon', hour=19, minute=56, replace_existing=True, max_instances=1)

# # Http server usado apenas para manter a aplicação de pé para o agendador aguardar e executar is jobs
# PORT = 8000
# Handler = http.server.SimpleHTTPRequestHandler
# with socketserver.TCPServer(("", PORT), Handler) as httpd:
#     print("serving at port", PORT)
#     httpd.serve_forever()

