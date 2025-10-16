(ns atividades.main)

(defn classificar-nota
  [nota]
  (cond
    (>= nota 90) "A"
    (>= nota 80) "B"
    (>= nota 70) "C"
    (>= nota 60) "D"
    :else "F"))

(defn avaliar-desempenho-turma
  [media]
  (if (>= media 80)
    "Turma excelente!"
    (if (>= media 60)
      "Bom desempenho!"
      "É necessário melhorar!")))

(defn ler-dados-aluno
  [numero-aluno]
  (print (str "Nome do aluno " numero-aluno ": "))
  (flush)
  (let [nome (read-line)]
    (print "Nota: ")
    (flush)
    (let [nota (Double/parseDouble (read-line))]
      {:nome nome :nota nota})))

(defn processar-alunos
  "Processa todos os alunos usando loop/recur"
  [quantidade-alunos]
  (loop [i 1
         notas []
         aprovados 0]
    (if (<= i quantidade-alunos)
      (let [aluno (ler-dados-aluno i)
            nome (:nome aluno)
            nota (:nota aluno)
            conceito (classificar-nota nota)]
        (println (str nome " - Conceito: " conceito))
        (recur (inc i)
               (conj notas nota)
               (if (>= nota 60) (inc aprovados) aprovados)))
      {:notas notas :aprovados aprovados})))

(defn -main []
  (print "Quantos alunos na turma? ")
  (flush)
  
  (let [quantidade-alunos (Integer/parseInt (read-line))
        resultado (processar-alunos quantidade-alunos)
        notas (:notas resultado)
        aprovados (:aprovados resultado)
        media (/ (reduce + notas) (count notas))
        desempenho (avaliar-desempenho-turma media)]
    
    (println)
    (println (str "Média da turma: " (format "%.1f" media)))
    (println (str "Aprovados: " aprovados))
    (println (str "Desempenho geral: " desempenho))))