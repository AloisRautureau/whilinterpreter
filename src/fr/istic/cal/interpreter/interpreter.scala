package fr.istic.cal.interpreter

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 *
 * ETUDIANT 1 : RAUTUREAU Aloïs
 *
 * ETUDIANT 2 : JUNG Claire
 *
 */

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case (name, value) :: next => if (name == v) value else lookUp(v, next)
      case Nil                   => NlValue
    }
  }
  
  /**
   * @param m1 : une mémoire
   * @param m2 : une autre mémoire
   * @return : une liste de variables différentes entre m1 et m2
   */
  def diff(m1: Memory, m2: Memory): List[Variable] = {
    m1.foldLeft(List[Variable]()){ case (acc, (variable, value)) => if(value != lookUp(variable, m2)) variable :: acc else acc }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire modifiée par l'affectation [v->d]
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match {
      case (entry @ (name, value)) :: next => {
        if (name == v) (name, d) :: next
        else entry :: assign(v, d, next)
      }
      case Nil => (v, d) :: Nil
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl               => NlValue
      case Cst(name)        => CstValue(name)
      case VarExp(name)     => lookUp(Var(name), mem)
      case Cons(arg1, arg2) => ConsValue(interpreterExpr(arg1, mem), interpreterExpr(arg2, mem))
      case Hd(arg) => interpreterExpr(arg, mem) match {
        case ConsValue(a, _) => a
        case _               => NlValue
      }
      case Tl(arg) => interpreterExpr(arg, mem) match {
        case ConsValue(_, b) => b
        case _               => NlValue
      }
      case Eq(arg1, arg2) => {
        if (interpreterExpr(arg1, mem) == interpreterExpr(arg2, mem)) CstValue("true")
        else NlValue
      }
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant l'expression de cette valeur
   */

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue               => Nl
      case CstValue(name)        => Cst(name)
      case ConsValue(arg1, arg2) => Cons(valueToExpression(arg1), valueToExpression(arg2))
    }
  }

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop                       => memory
      case Set(variable, expression) => assign(variable, interpreterExpr(expression, memory), memory)
      case While(condition, body) => {
        if (interpreterExpr(condition, memory) != NlValue) {
          interpreterCommand(command, interpreterCommands(body, memory))
        } 
        else memory
      }
      case For(count, body) => {
        interpreterExpr(count, memory) match {
          case ConsValue(_, next) => interpreterCommand(For(valueToExpression(next), body), interpreterCommands(body, memory))
          case CstValue(_)        => interpreterCommands(body, memory)
          case _                  => memory
        }
      }
      case If(condition, then_cmds, else_cmds) => {
        if (interpreterExpr(condition, memory) != NlValue) interpreterCommands(then_cmds, memory)
        else interpreterCommands(else_cmds, memory)
      }
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    if (commands.isEmpty) throw ExceptionListeVide
    commands.foldLeft(memory)( (mem, cmd) => interpreterCommand(cmd, mem) )
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    if (vars.length != vals.length) throw ExceptionListesDeLongueursDifferentes
    if (vars.isEmpty || vals.isEmpty) throw ExceptionListeVide

    var memory: Memory = Nil
    (vars zip vals).foldLeft(memory) { case (mem, (variable, value)) => assign(variable, value, mem) }
  }

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    if (vars.isEmpty) throw ExceptionListeVide
    vars.map(lookUp(_, memory))
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
    program match {
      case Progr(in, body, out) => {
        interpreterMemoryGet(
          out,
          interpreterCommands(
            body,
            interpreterMemorySet(in, vals)))
      }
    }
  }

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

}