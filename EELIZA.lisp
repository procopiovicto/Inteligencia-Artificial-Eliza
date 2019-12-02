(defun simple-equal (x y)
  (if (or (atom x) (atom y))
	(eql x y)
	(and (simple-equal (car x) (car y))
		 (simple-equal (cdr x) (cdr y)))))

(defun pat-match (pattern input)
  (if (variable-p pattern)
	t
	(if (or (atom pattern ) (atom input))
	  (eql pattern input)
	  (and (pat-match (car pattern) (car input))
		   (pat-match (cdr pattern) (cdr input))))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val) bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern)
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input)
					(pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
	(cond ((not binding) (extend-bindings var input bindings))
		  ((equal input (binding-val binding)) bindings)
		  (t fail))))


(defun extend-bindings (var val bindings)
  (cons (cons var val)
		(if (eq bindings no-bindings)
		  nil
		  bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern) 
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((segment-pattern-p pattern)
		 (segment-match pattern input bindings))
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input)
					(pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
	   (starts-with (car pattern) '?*)))

(defun starts-with (lst symb)
  (if (consp lst)
	(eql (car lst) symb)
	(eql lst symb)))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadr (car pattern)))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable var input bindings)
	  (let ((pos (position (car pat) input :start start :test #'equal)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match pat (subseq input pos) bindings)))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  (match-variable var (subseq input 0 pos) b2))))))))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadr (car pattern)))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable var input bindings)
	  (let ((pos (position (car pat) input :start start :test #'equal)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match
					  pat (subseq input pos)
					  (match-variable var (subseq input 0 pos)
									  bindings))))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  b2)))))))

(defun rule-pattern (rule) (car rule))
(defun rule-responses (rule) (cdr rule))

(defparameter *eliza-rules* 
 '((((?* ?x) ola (?* ?y)) ;;;;;;;;;;;;;;; SAUDAÇÕES
    (Sobre o que voce gostaria de falar? Ansiedade? Depressao? Sindrome de Panico? Complexo de Inferioridade? Transtorno Bipolar? Sindrome de Burnout? Outros Problemas?))
   (((?* ?x) oi (?* ?y))
    (Sobre o que voce gostaria de falar? Ansiedade? Depressao? Sindrome de Panico? Complexo de Inferioridade? Transtorno Bipolar? Sindrome de Burnout? Outros Problemas))
   
   (((?* ?x) estou (?* ?y)) ;;;;;;;;;;;;;;; ESTADO
    (Como voce esta?))     
   (((?* ?x) (eu estou triste) (?* ?y))
    (O que te deixa triste?))
   (((?* ?x) (eu estou estressado) (?* ?y))
    (O que te deixa estressado?))
   (((?* ?x) (eu estou zangado) (?* ?y))
    (O que te deixa zangado?))
   (((?* ?x) (eu estou cansado) (?* ?y))
    (O que te deixa cansado?))
   (((?* ?x) (eu estou estressada) (?* ?y))
    (O que te deixa estressada?))
   (((?* ?x) (eu estou zangado) (?* ?y))
    (O que te deixa zangada?))
   (((?* ?x) (eu estou cansado) (?* ?y))
    (O que te deixa cansada?))
   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIO DO BLOCO DEPRESSAO 
   
   (((?* ?x) depressao (?* ?y)) 
     (Sobre qual sintoma gostaria de falar? Tristeza constante? Cansaco e Insonia? Perda de interesse por atividades que antes apreciava? Insatisfacao com a vida? Perda de peso? Irritabilidade?))
   (((?* ?x) tristeza (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) (tristeza constante) (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) cansaco (?* ?y))
     ( faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) Insonia (?* ?y))
     (faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) (cansaco e insonia) (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) (perda de interesse por atividades que antes apreciava) (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! E normal se sentir assim... esta bem?))
     (((?* ?x) (perda de interesse por atividades)(?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! E normal se sentir assim... esta bem?))
     (((?* ?x) (perda de interesse)(?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! E normal se sentir assim... esta bem?))
     (((?* ?x) (insatisfacao com a vida) (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) insatisfacao (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) (Perda de peso) (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
     (((?* ?x) irritabilidade (?* ?y))
     (Saiba que nao e sua culpa e que eu apoio voce! faca algo que goste...leia um livro...va ao cinema...tente nao pensar nisso...esta bem?))
   
   
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CASO A PESSOA RESPONDA 'ESTA BEM'
           
     (((?* ?x)(esta bem) (?* ?y))
     (Otimo! Sempre lembre que voce e importante! Gostaria de falar sobre outro sintoma?))
     (((?* ?x)(ta bem) (?* ?y))
     (Otimo! Sempre lembre que voce e importante! Gostaria de falar sobre outro sintoma?))
     (((?* ?x) esta (?* ?y))
     (Otimo! Sempre lembre que voce e importante! Gostaria de falar sobre outro sintoma?))
     (((?* ?x) ok (?* ?y))
     (Otimo! Sempre lembre que voce e importante! Gostaria de falar sobre outro sintoma?))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CASO A PESSOA RESPONDA 'NAO ESTA'

    (((?* ?x)(nao esta bem) (?* ?y))
    (Poxa! Voce pode nao acreditar agora mas voce ira ficar bem! Sempre lembre que voce e importante Gostaria de falar sobre outro sintoma?))
    (((?* ?x)(nao esta) (?* ?y))
    (Poxa! Voce pode nao acreditar agora mas voce ira ficar bem! Sempre lembre que voce e importante Gostaria de falar sobre outro sintoma?))
    (((?* ?x)(nao esta bem) (?* ?y))
    (Poxa! Voce pode nao acreditar agora mas voce ira ficar bem! Sempre lembre que voce e importante Gostaria de falar sobre outro sintoma?))
    (((?* ?x) nao (?* ?y))
    (Poxa! Voce pode nao acreditar agora mas voce ira ficar bem! Sempre lembre que voce e importante Gostaria de falar sobre outro sintoma?))

    ;;;;;;;;;;;;;;;; CASO A PESSOA RESPONDA 'GOSTARIA OU NAO GOSTARIA'

    (((?* ?x) gostaria (?* ?y))
    (Sobre qual sintoma gostaria de falar? Tristeza constante? Cansaco e Insonia? Perda de interesse por atividades que antes apreciava? Insatisfacao com a vida? Perda de peso? Irritabilidade?))
    (((?* ?x) (nao gostaria) (?* ?y))
    (Ok! muito obrigada pela consulta!))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIM DO BLOCO DEPRESSAO

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIO DO BLOCO TRANSTORNO BIPOLAR
   
    (((?* ?x) (transtorno bipolar) (?* ?y)) 
     (Sobre qual sintoma gostaria de falar? Distracao? Alteracooes de humor? Pensamento acelerado? Agitacao?))
     (((?* ?x) Distracao (?* ?y))
     (Faca uma pausa e de uma volta... mas num lugar com natureza... Isso ajuda a reduzir a ansiedade e a aumentar a capacidade mental! certo?))
     (((?* ?x) (Alteracoes de humor) (?* ?y))
     (Descubra o faz voce se sentir melhor e crie sua propria rotina incluindo esta atividade. Por exemplo: musica... um passeio... uma conversa animada com amigos… Identifique as atividades que o acalmam e tente sempre ter um espaco na agenda para elas.))
     (((?* ?x) (Pensamento acelerado) (?* ?y))
     (Que tal pensar nos seus problemas como se eles fossem de outra pessoa! assim fica mais facil deixar de se incomodar com eles. Apenas respire e mantenha o foco no presente. ^^))
     (((?* ?x) Agitacao (?* ?y))
     (Pratique exercicios... durma bastante e alimente-se de maneira correta! isso ajuda a diminuir os niveis de estresse e a relaxar ok?))
   
 

   

   ;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIM DO BLOCO TRANSTORNO BIPOLAR
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIO DO BLOCO COMPLEXO DE INFERIORIDADE
   
    (((?* ?x) (complexo de inferioridade) (?* ?y)) 
     (Sobre qual sintoma gostaria de falar? isolamento? Hábito de se comparar com os outros? Sentimentos de incapacidade e de inferioridade? Preocupação excessiva com a opinião das pessoas?))
     (((?* ?x) isolamento (?* ?y))
     (Faca uma pausa e de uma volta... mas num lugar com natureza... Isso ajuda a refletir sobre a sua própria identidade! certo?))
     (((?* ?x) (Hábito de se comparar com os outros) (?* ?y))
     (Descubra suas qualidades Ninguém é perfeito. Você pode até não ser tão bom em algumas coisas, especialmente quando se compara com algum especialista ou modelo, mas certamente possui características positivas que a outra pessoa não tem.  Toda vez que você se compara com alguém, você anula suas qualidades e particularidades.))
     (((?* ?x) (Sentimentos de incapacidade e de inferioridade) (?* ?y))
     (Escreva sobre seus sucessos ao longo da vida, as realizações das quais se orgulha e memórias queridas. Quando se sentir incapaz de fazer alguma coisa, veja sua lista e lembre-se de como você é uma pessoa capaz e fantástica.^^))
     (((?* ?x) Preocupação excessiva com a opinião das pessoas (?* ?y))
     (Tire o foco das pessoas e comece a dar impotância a pessoa maravilhosa que você é.Pratique exercicios... durma bastante e alimente-se de maneira correta!Se desenvolva como pessoa e se torne alguém ainda melhor,ok?))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIO DO BLOCO SINDROME DE PANICO

   (((?* ?x) (sindrome de panico) (?* ?y)) 
   (Sobre qual sintoma gostaria de falar? Dor no peito com sensacao de aperto? Sensacao de falta de ar? Sensacao de fraqueza ou desmaio? Sensacao de terror ou perigo iminente?))
   (((?* ?x) (Dor no peito com sensacao de aperto) (?* ?y))
   (Respirar lenta e profundamente ajuda a reduzir a frequencia cardiaca tudo bem?))
   (((?* ?x) (Sensacao de falta de ar) (?* ?y))
   (Sente-e com a coluna ereta ou fique de pe com o corpo reto... feche os olhos e coloque as maos sobre a barriga... inspire contando ate 5 lentamente... estufando a barriga para enche-la de ar... expire tambem contando ate 5 lentamente... liberando o ar da barriga e contraindo os musculos desta regiao ok?))
   (((?* ?x) (Sensacao de fraqueza ou desmaio) (?* ?y))
   (Voce ja pensou em fazer yoga eh uma pratica que une alongamentos... controle da respiração e fortalecimento da musculatura. Ok?))
   (((?* ?x) (Sensacao de terror ou perigo iminente) (?* ?y))
   (Imagine um local real que transmita paz e segurança ou crie um ambiente imaginario... pensando em todos os detalhes que ajudam a trazer tranquilidade tudo bem?))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIM DO BLOCO SINDROME DE PANICO


   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; ZUERA NOSSA DE CADA DIA
   (((?* ?x) (bora beber) (?* ?y))
     (Soh bebo agua major)
     (Me chame n que eu vou))
   (((?* ?x) (vamos beber) (?* ?y))
     (Soh bebo agua major)
     (Me chame n que eu vou))
   (((?* ?x) (vms beber) (?* ?y))
     (Soh bebo agua major)
     (Me chame n que eu vou))
   (((?* ?x) (tenho fome) (?* ?y))
     (To azilada de fome tbm paga um rango p nos))
   (((?* ?x) fome (?* ?y))
     (To azilada de fome tbm paga um rango p nos))
   (((?* ?x) (eu tenho fome) (?* ?y))
     (To azilada de fome tbm paga um rango p nos))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;; INICIO DO BLOCO OUTROS PROBLEMAS
   (((?* ?x) (outros problemas) (?* ?y))
      (Por qual problema voce ta passando? Eh por causa do seu corpo? ou por causa do seu namoro? Por causa do emprego? ou por causa da familia?))
   
  (((?* ?x) (outros) (?* ?y))
      (Por qual problema voce ta passando? Eh por causa do seu corpo? ou por causa do seu namoro? Por causa do emprego? ou por causa da familia?))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; familia
(((?* ?x) familia (?* ?y))
      (o que ta acontecendo na sua familia? eh algo com seus pais ou irmao?))
(((?* ?x) pais (?* ?y))
      (o que aconteceu com seus pais? eles brigaram ou acabaram se separando?))
(((?* ?x) (meus pais brigaram) (?* ?y))
      (qual o motivo pelo qual seus pais brigaram? me conte mais sobre isso))
(((?* ?x) ciumes (?* ?y))
      (diga para seus pais que eles nao precisam sentir ciumes  supere tudo isso as coisas vao da certo. Voce tem mais algum problema?))
(((?* ?x) (meus pais se separaram) (?* ?y))
      (infelizmente voce nao pode mudar a vida de seus pais siga em frente pq eles estao fazendo isso. Voce tem mais algum problema?))
(((?* ?x) irmao (?* ?y))
      (o que aconteceu com seu irmao?))
(((?* ?x) irma (?* ?y))
      (o que aconteceu com seu irmao?))
(((?* ?x) (briguei com meu irmao) (?* ?y))
      (supere isso irmaos brigam mesmo e isso eh natural. Voce tem mais algum problema?))
(((?* ?x) (briguei com minha irma) (?* ?y))
      (supere isso irmaos brigam mesmo e isso eh natural. Voce tem mais algum problema?))
(((?* ?x) doente (?* ?y))
      (fique calmo pq as coisas vao da certo fique ao lado dele que logo logo a saude dele vai melhorar tenha fe. Voce tem mais algum problema?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; namoro
(((?* ?x) namoro (?* ?y))
      (Voce tem namorado ou namorada?))
(((?* ?x) namorado (?* ?y))
      (o que aconteceu com seu namorado?))
(((?* ?x) namorada (?* ?y))
      (o que aconteceu com sua namorada?))
(((?* ?x) (briguei com minha namorada) (?* ?y))
      (o que aconteceu com seu namorado?))
(((?* ?x) (briguei com meu namorado) (?* ?y))
      (fique calmo e converse com seu namorado as coisas se resolvem na conversa. Voce tem mais algum problema?))
(((?* ?x) (terminei com meu namorado) (?* ?y))
      (que triste mas infelizmente nao era pra ser entao siga em frente o mundo esta cheio de pessoas maravilhosas.Voce tem mais algum problema?))
(((?* ?x) (terminei com minha namorada) (?* ?y))
      (que triste mas infelizmente nao era pra ser entao siga em frente o mundo esta cheio de pessoas maravilhosas.Voce tem mais algum problema?))
(((?* ?x) (meu namorado me traiu) (?* ?y))
      (poxa me desculpe mas ele eh um idiota converse com ele sobre isso e diga o quanto chateado voce esta. Voce tem mais algum problema?))
(((?* ?x) (minha namorada me traiu) (?* ?y))
      (poxa me desculpe mas ela eh uma idiota converse com ela sobre isso e diga o quanto chateado voce esta. Voce tem mais algum problema?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; corpo

(((?* ?x) corpo (?* ?y))
      (qual problema com seu corpo? voce se sente muito magra? voce se sente muito gorda? Voce se acha muito alta ou baixa? tem problemas com acne?))
(((?* ?x) magra (?* ?y))
      (poxa nao precisa se sentir assim voce eh linda)
      (fique calma voce eh linda))
(((?* ?x) gorda (?* ?y))
      (poxa nao precisa se sentir assim voce eh linda)
      (fique calma voce eh linda))
(((?* ?x) alta (?* ?y))
      (poxa nao precisa se sentir assim voce eh linda)
      (fique calma voce eh linda))
(((?* ?x) magra (?* ?y))
      (poxa nao precisa se sentir assim voce eh linda)
      (fique calma voce eh linda))
(((?* ?x) acne (?* ?y))
      (poxa nao precisa se sentir assim voce eh linda)
      (fique calma voce eh linda))
(((?* ?x) (pensei em me suicidar) (?* ?y))
      (fique calmo e respira fundo me conte o pq que voce pensou em fazer isso. Quem te deixa triste eh a faculdade? seu corpo? sua familia?)
      (voce eh incrivel e nao pode fazer isso))
(((?* ?x) (a faculdade me deixa triste) (?* ?y))
      (pq a faculdade te deixa triste?)
      (me conte mais sobre isso. Pq a faculdade te deixa triste? sao suas notas?))
(((?* ?x) (notas) (?* ?y))
      (fique calmo. Tirar notas baixas eh normal. Pq vc acha que tira notas baixas?Voce nao consegue estudar?)
      (calma que no final da tudo certo. Pq vc acha que tira notas baixas?Voce nao consegue estudar?))
(((?* ?x) (nao consigo estudar) (?* ?y))
      (quando for estudar respire fundo e estude no seu tempo. Tudo vai da certo)
      (Pq voce nao consegue estudar?))
(((?* ?x) (nao gosto de estudar) (?* ?y))
      (voce tem que entender que estudar eh muito importante para o seu futuro. Respire fundo e tente mais uma vez))
(((?* ?x) (nao gosto da faculdade) (?* ?y))
      (poxa cara... acho que ninguem gosta mas todos precisamos dela)
      (se voce nao gosta da faculdade ja pensou em trocar de curso?))
(((?* ?x) (ja pensei em trocar de curso) (?* ?y))
      (serio? qual curso voce quer fazer?))
(((?* ?x) medicina (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) odontologia (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) (arquitetura) (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) financas (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) economia (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) (engenharia eletrica) (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) musica (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) psicologia (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) enfermagem (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) letras (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) (medicina veterinaria) (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) direito (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) (engenharia civil) (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) matematica (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) agronomia (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) (ciencias da computacao) (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
(((?* ?x) (engenharia de software) (?* ?y))
      (segue teu sonho)
      (vai seguir teu sonho eu sei que voce vai conseguir))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primeiro bloco

   (((?* ?x) (eu sonhei) (?* ?y))
     (Realmente?) (Que tipo de sonho voce teve em relacao a faculdade?)
     (Você já teve esse tipo de sonho? Era bom ou ruim?))
   (((?* ?x) (sonhar com) (?* ?y))
     (Como você se sente sobre? Tenho certeza se fosse um sonho com física seria um maximo...Rs rs))
   (((?* ?x) sonhei (?* ?y))
     (O sonho foi com alguma disciplina específica desse semestre? Qual?) (Que professor costuma entrar em seu sonho?)  
     (Você não acredita que o sonho tem a ver com o seu problema?))
   

   (((?* ?x) (minha familia) (?* ?y))
     (Nao tenho interesse em familia))
   (((?* ?x) (minha mãe) (?* ?y))
     (Nao estou interessada em membros da familia) (Vamos focar no seu ambiente de estudo)
     (Cite algo que te incomoda))
   (((?* ?x) (meu pai) (?* ?y))
     (Seu pai) (nao estou interessada nele))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Segundo bloco
 
   (((?* ?x) (eu quero) (?* ?y))
     (O que significaria se voce tivesse?)
     (Por que voce quer? ) (Suponha que voce tenha em breve))
   (((?* ?x) (estou feliz) (?* ?y))
     (Como eu te ajudei a ser?) (O que te faz feliz agora?)
     (Voce pode explicar por que voce ficou feliz?))
   (((?* ?x) (estou triste) (?* ?y))
     (Lamento saber que voce esta deprimido)
     (Tenho certeza que nao eh agradavel estar triste))
  

   (((?* ?x) (sao como) (?* ?y))
     (Que semelhança voce ver entre os professores?)(Tem algum em especial? Qual?)
     (Interessante.)(Voce acha que esse semestre esta mais facil ou dificil que o passado?))
   (((?* ?x) (eh como) (?* ?y))
     (De que maneita paradigmas eh como arquitetura?)
     (Que semelhança você vê?)
     (Poderia haver realmente alguma conexão?) (Como?))
   (((?* ?x) igualmente (?* ?y))
     (De que maneira?) (Quais semelhancas existem?))
   (((?* ?x) mesmo (?* ?y))
     (Que outras conexoes voce ver?))   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Terceiro bloco


   (((?* ?x) (eu estava) (?* ?y))
     (Voce estava mesmo?) (Talvez eu ja soubesse disso)
     (Por que voce me diz que voce estava agora?))
   (((?* ?x) (eu era) (?* ?y))
     (E se você fosse?) (Voce esta mais feliz?)
     (O que significaria se voce fosse?))
   (((?* ?x) (eu sou) (?* ?y))
     (De que maneira voce eh?) (Voce quer que eu saiba?))
   (((?* ?x) (sou eu) (?* ?y))
     (Voce acredita que eh?) (Voce gostaria de ser?)
     (Voce gostaria de lhe dizer que eh?)
     (O que significaria se voce fosse?))
   (((?* ?x) am (?* ?y))
     (Por que voce diz "AM?") (Eu nao entendo isso))
   (((?* ?x) (voce esta?)(?* ?y))
     (Por que voce esta interessado em saber se eu estou ou nao?)
     (Voce preferiria se eu nao estivesse?)
     (Talvez eu esteja em suas fantasias))
   (((?* ?x) (voce eh?) (?* ?y))
     (O que faz voce pensar que eu sou?))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quarto bloco


   (((?* ?x) (porque sim) (?* ?y))
      (Esse eh o verdadeiro motivo?) (Quais outras razoes podem existir?)
      (Essa razao parece explicar mais alguma coisa?))
   (((?* ?x) (voce estava) (?* ?y))
      (Talvez eu estivesse) (O que voce acha?) (E se eu tivesse?))
   (((?* ?x) (eu nao posso) (?* ?y))
      (Talvez voce pudesse agora)(E se voce pudesse?))
   (((?* ?x) (eu sinto) (?* ?y))
      (Voce costuma sentir? ?))
   (((?* ?x) (eu senti) (?* ?y))
      (Que outros sentimentos você tem?)
      (Por que voce sentiu?)) 
   (((?* ?x) eu (?* ?y) voce (?* ?z))
      (Talvez na sua fantasia nos? Um ao outro...calientes))
   (((?* ?x) (por que voce nao?) (?* ?y))
      (Talvez apenas voce consiga)(Acredite mais em voce mesmo)
      (Você acredita que eu nao?) (Talvez eu va? Em tempo util))
   (((?* ?x) sim (?* ?y))
      (Voce parece bastante positivo) (O que motiva voce?))
   (((?* ?x) nao (?* ?y))
      (Esse seu nao alem disso tem mais algum motivo especial?)  
      (Por que nao?) (Voce esta sendo um pouco negativo)
      (Voce esta dizendo "NAO" apenas para ser negativo?))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quinto bloco

   (((?* ?x) alguem (?* ?y))
      (Voce pode ser mais especifico?))
   (((?* ?x) todos (?* ?y))
      (certamente nao todos) (Voce pode pensar em algo ou alguem em particular?)
      (Quem ou o que por exemplo?) (Voce esta pensando em uma pessoa especial))
   (((?* ?x) sempre (?* ?y))
      (Voce pode pensar em um exemplo específico) (Quando?)
      (Que incidente voce esta pensando?) (Realmente-- sempre))
   (((?* ?x) (o que?) (?* ?y))
      (Por que voce pergunta?) (Essa pergunta lhe interessa?)
      (O que voce realmente quer saber?) (O que voce acha?)
      (O que vem a sua mente quando voce pergunta isso?))
   (((?* ?x) talvez (?* ?y))
      (Voce não parece muito certo))
   (((?* ?x) sao (?* ?y))
      (Voce acha que podem nao ser?)
      (Possivelmente sao?))

   (((?* ?x) (mais facil) (?* ?y))
     (Interessante.)
     (Me conte mais.))

   (((?* ?x) (mais facil) (?* ?y))
     (Interessante.)
     (Me conte mais.))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPREGO
(((?* ?x) emprego (?* ?y))
      (qual o problema com seu trabalho? voce nao gosta dele? voce nao gosta do seu chefe? voce ganha pouco dinheiro?))
(((?* ?x) (nao gosto do meu trabalho) (?* ?y))
      (poxa cara.Ja pensou em trocar de trabalho?))
(((?* ?x) (nao gosto do meu chefe) (?* ?y))
      (poxa cara.Infelizmente voce nao pode fazer nada para mudar isso))
(((?* ?x) (ganho pouco dinheiro) (?* ?y))
      (poxa cara. Infelizmente todo mundo passa por esse porblema. Tente trocar de empregou ou ser promovido))
(((?* ?x) (preciso de dinheiro) (?* ?y))
      (poxa cara.Infelizmente todo mundo passa por esse porblema.))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIM DO BLOCO OUTROS PROBEMAS

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRATAMENTO DE EXCEÇÃO
(((?* ?x))
     (Muito interessante continue) (nao tenho certeza se entendi totalmente)
     (O que isso sugere para você?) (por favor continue) (vai em frente)
     (Voce se sente forte sobre discutir essas coisas?))))

(defun eliza ()
  (loop
	(print 'eliza>)
	(write (flattern (use-eliza-rule (read))) :pretty t)))

(defun use-eliza-rule (input)
  (some #'(lambda (rule)
			(let ((result (pat-match (rule-pattern rule) input)))
			  (if (not (eq result fail))
				(sublis (switch-viewpoint result)
						(random-elt (rule-responses rule))))))
		*eliza-rules*))
(defun switch-viewpoint (words)
  (sublis '((eu . voce) (voce . eu) (mim . voce) (sou . esta))
		  words))

(defun random-elt (lst)
  (elt lst (random (length lst))))

(defun mappend (fn lst)
  (apply #'append (mapcar fn lst)))

(defun flattern (lst)
  (mappend #'mklist lst))

(defun mklist (x)
  (if (listp x)
	x
	(list x)))

(eliza)


