# Modelos Probabilistas Aplicados
# Tarea 12
# Tema: Problemas teoricos - practicos
# Johanna Bolaños Zuñiga


pj = function(j){
    return( exp(-2) * 2 ** j / factorial(j) ) #funcion dada
}

d = pj(0)
for (m in 1:2000){
    d1 = 0
    for (j in 0:50){
        d1= d1 + pj(j) * (d ** j)
    }
    d = d1
    
}

