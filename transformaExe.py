import subprocess
import sys
import os

def get_dir():
    if getattr(sys, 'frozen', False):  # Está rodando como exe
        return os.path.dirname(sys.executable)
    else:  # Está rodando como script Python
        return os.path.dirname(os.path.abspath(__file__))

diretorio_atual = get_dir()
caminho_r = os.path.join(diretorio_atual, "main.R")

subprocess.call(["Rscript", caminho_r])
