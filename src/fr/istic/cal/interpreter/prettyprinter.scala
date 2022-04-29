package fr.istic.cal.interpreter

import scala.util.Try

object Prettyprinter {
  
  /**
   * UN PRETTY-PRINTER POUR LE LANGAGE WHILE
   *
   */

  /**
   * définition d'un type pour les spécifications d'indentation
   */
  type IndentSpec = List[(String, Int)]
  
  /**
   * définition d'une valeur d'indentation par défaut
   */
  val indentDefault: Int = 1

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une chaîne représentant la syntaxe concrète de l'expression
   */
  def prettyPrintExpr(expression: Expression): String = {
    expression match {
      case Nl => "nil"
      case Cst(name) => name
      case VarExp(name) => name
      case Cons(arg1, arg2) => "(cons " + prettyPrintExpr(arg1) + " " + prettyPrintExpr(arg2) + ")"
      case Hd(arg) => "(hd " + prettyPrintExpr(arg) + ")"
      case Tl(arg) => "(tl " + prettyPrintExpr(arg) + ")"
      case Eq(arg1, arg2) => prettyPrintExpr(arg1) + " =? " + prettyPrintExpr(arg2)
    }
  }

  
  /**
   *  FONCTIONS AUXILIAIRES DE TRAITEMENT DE CHAINES POUR L'INDENTATION DES COMMANDES
   *  OU LA PRESENTATION DU PROGRAMME
   */

  /**
   * recherche d'une valeur d'indentation dans une liste de spécifications d'indentation
   *
   * @param context une chaîne de caractères décrivant un contexte d'indentation
   * @param is une liste de spécifications d'indentation, chaque spécification étant un couple (un contexte,une indentation)
   * les contextes possibles seront, en majuscules, "WHILE", "FOR", "IF", ou "PROGR". 
   * @return l'indentation correspondant à context
   */
  def indentSearch(context: String, is: IndentSpec): Int = {
    is match {
      case Nil => indentDefault
      case (current, value)::next => if (current == context) value else indentSearch(context, next)
    }
  }

  
  /**
   * création d'une indentation
   *
   * @param n un nombre d'espaces
   * @return une chaîne de n espaces
   */
  def makeIndent(n: Int): String = ' '.toString * n

  
  /**
   * ajout d'une chaîne devant chaque élément d'une liste non vide de chaînes
   *
   * @param pref une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de pref devant chaque élément de strings
   */
  def appendStringBeforeAll(pref: String, strings: List[String]): List[String] = strings.map( s => pref + s )

  
  /**
   * ajout d'une chaîne après chaque élément d'une liste non vide de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings
   */
  def appendStringAfterAll(suff: String, strings: List[String]): List[String] = strings.map( s => s + suff )

  /**
   * ajout d'une chaîne après chaque élément d'une liste non vide de chaînes sauf le dernier
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après chaque élément de strings sauf le dernier
   */
  def appendStringAfterAllButLast(suff: String, strings: List[String]): List[String] = {
    strings match {
      case last::Nil => strings 
      case str::next => str + suff :: appendStringAfterAllButLast(suff, next)
      case Nil => throw ExceptionListeVide
    }
  }

  
  /**
   * ajout d'une chaîne après le dernier élément d'une liste non vide de chaînes
   *
   * @param suff une chaîne
   * @param strings une liste non vide de chaînes
   * @return une liste de chaînes obtenue par la concaténation de suff après le dernier élément de strings
   */
  def appendStringAfterLast(suff: String, strings: List[String]): List[String] = {
    strings.take(strings.length - 1) :+ (strings.last + suff)
  }

  /**
   * Ajout de l'indentation correspondant à un contexte sur une liste de chaînes
   * 
   * @param context une chaîne décrivant le contexte ("FOR", "WHILE", "IF" our "PROGR")
   * @is la spécification d'indentation donnée
   * @strings la liste de chaînes à indenter
   */
  def addIndent(context: String, is: IndentSpec, strings: List[String]): List[String] = {
    appendStringBeforeAll( makeIndent( indentSearch(context, is) ), strings )
  }
  
  
  
  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la commande
   */
  // TODO TP2
  def prettyPrintCommand(command: Command, is: IndentSpec): List[String] = {
    command match {
      case Nop => "nop" :: Nil
      case Set(Var(name), expression) => name + " := " + prettyPrintExpr(expression) :: Nil
      case While(condition, body) => {
        ( "while " + prettyPrintExpr(condition) + " do" :: addIndent("WHILE", is, prettyPrintCommands(body, is)) ) :+ "od"
      }
      case For(count, body) => {
        ( "for " + prettyPrintExpr(count) + " do" :: addIndent("FOR", is, prettyPrintCommands(body, is)) ) :+ "od" 
      }
      case If(condition, then_cmd, else_cmd) => {
        ( "if " + prettyPrintExpr(condition) + " then" :: addIndent("IF", is, prettyPrintCommands(then_cmd, is)) ) ::: ( ( "else" :: addIndent("IF", is, prettyPrintCommands(else_cmd, is)) ) :+ "fi" )
      }
    }
  }

  
  /**
   * @param command : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète de la liste de commandes
   */
  def prettyPrintCommands(commands: List[Command], is: IndentSpec): List[String] = {
    if (commands == Nil) throw ExceptionListeVide
    commands.take(commands.length - 1).foldLeft( List[String]() )( (acc, cmd) => acc ::: appendStringAfterLast(" ;", prettyPrintCommand(cmd, is)) ) ::: prettyPrintCommand(commands.last, is)
  }

  
  
  
  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les paramètres d'entrée d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres d'entrée du programme
   */
  def prettyPrintIn(vars: List[Variable]): String = {
    vars match {
      case Nil => throw ExceptionListeVide
      case Var(name)::Nil => name
      case Var(name)::next => name + ", " + prettyPrintOut(next)
    }
  }
  
  
  /**
   * @param vars : une liste non vide décrivant les paramètres de sortie d'un programme du langage WHILE
   * @return une liste de chaînes représentant la syntaxe concrète des paramètres de sortie du programme
   */
  def prettyPrintOut(vars: List[Variable]): String = {
    vars match {
      case Nil => throw ExceptionListeVide
      case Var(name)::Nil => name
      case Var(name)::next => name + ", " + prettyPrintOut(next)
    }
  }

  
  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  def prettyPrintProgram(program: Program, is: IndentSpec): List[String] = {
    program match {
      case Progr(in, body, out) => ( "read " + prettyPrintIn(in) :: "%" :: addIndent("PROGR", is, prettyPrintCommands(body, is)) ) ::: "%" :: "write " + prettyPrintOut(out) :: Nil
    }
  }

  
  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une chaîne représentant la syntaxe concrète du programme
   */
  def prettyPrint(program: Program, is: IndentSpec): String = {
    appendStringAfterAllButLast("\n", prettyPrintProgram(program, is)).foldLeft( "" )( (acc, line) => acc + line )
  }

  
  
  val program: Program =
    Progr(
      List(Var("X")),
      List(
        Set(Var("Y"), Nl),
        While(
          VarExp("X"),
          List(
            Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
            Set(Var("X"), Tl(VarExp("X")))))),
      List(Var("Y")));
  val is: IndentSpec = List(("PROGR", 2), ("WHILE", 5));

  def main(args: Array[String]): Unit = {
   println(prettyPrint(program, is));
  }
  
  
  
    /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite 
   * respectivement pour une expression, une commande, un programme
   */
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s : String) : Expression = WhileParser.analyserexpression(s)
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s : String) : Command= WhileParser.analysercommand(s)  
  
  /**
   * @param s : une chaine de caractère représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s : String) : Program = WhileParser.analyserprogram(s)

}