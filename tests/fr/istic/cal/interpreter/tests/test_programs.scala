package fr.istic.cal.interpreter.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.interpreter.Interpreter._
import fr.istic.cal.interpreter._

class TestsPrograms {

  @Test
  def Test_interpreterMemorySet_1(): Unit = {
    val expected = List((Var("X"), CstValue("xxx")))
    val result = interpreterMemorySet(List(Var("X")), List(CstValue("xxx")))
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)) && expected.length == result.length)
  }

  @Test
  def Test_interpreterMemorySet_2(): Unit = {
    val expected = List((Var("Z"), NlValue), (Var("Y"), CstValue("yyy")), (Var("X"), CstValue("xxx")))
    val result = interpreterMemorySet(List(Var("X"), Var("Y"), Var("Z")), List(CstValue("xxx"), CstValue("yyy"), NlValue))
    // println("expected : " + expected)
    // println("was      : " + result)
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)) && expected.length == result.length)
  }

  @Test
  def test_interpreterMemorySet__3(): Unit = {
    try {
      interpreterMemorySet(Nil,Nil);
      fail();
    } catch {
      case ExceptionListeVide => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
   @Test
  def test_interpreterMemorySet__4(): Unit = {
    try {
      interpreterMemorySet(List(Var("X"), Var("Y")),List(CstValue("xxx"), CstValue("yyy"), NlValue));
      fail();
    } catch {
      case ExceptionListesDeLongueursDifferentes => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
  @Test
  def test_interpreterMemorySet__5(): Unit = {
    try {
      interpreterMemorySet(Nil,List(CstValue("xxx"), CstValue("yyy"), NlValue));
      fail();
    } catch {
      case ExceptionListesDeLongueursDifferentes => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
  @Test
  def test_interpreterMemorySet__6(): Unit = {
    try {
      interpreterMemorySet(List(Var("X"), Var("Y"), Var("Z")),Nil);
      fail();
    } catch {
      case ExceptionListesDeLongueursDifferentes => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  

  @Test
  def Test_interpreterMemoryGet_1(): Unit = {
    assertEquals(
      List(CstValue("xxx")),
      interpreterMemoryGet(List(Var("X")), List((Var("Z"), NlValue), (Var("Y"), CstValue("yyy")), (Var("X"), CstValue("xxx")))))
  }

  @Test
  def Test_interpreterMemoryGet_2(): Unit = {
    assertEquals(
      List(CstValue("xxx"), NlValue),
      interpreterMemoryGet(List(Var("X"), Var("Z")), List((Var("Z"), NlValue), (Var("Y"), CstValue("yyy")), (Var("X"), CstValue("xxx")))))
  }

  @Test
  def test_interpreterMemoryGet__3(): Unit = {
    try {
      interpreterMemoryGet(Nil,Nil);
      fail();
    } catch {
      case ExceptionListeVide => () // Rattrape uniquement l'exception déclarée, et levée explicitement
      case exn: MatchError         => () // Rattrape l'exception matchError levée implicitement
    }
  }
  
  @Test
  def Test_interpreter(): Unit = {
    val reverse: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))))),
        List(Var("Y")))
    assertEquals(
      List(ConsValue(CstValue("ddd"), ConsValue(CstValue("ccc"), ConsValue(CstValue("bbb"), ConsValue(CstValue("aaa"), NlValue))))),
      interpreter(
        reverse,
        List(ConsValue(CstValue("aaa"), ConsValue(CstValue("bbb"), ConsValue(CstValue("ccc"), ConsValue(CstValue("ddd"), NlValue)))))))
  }

  @Test
  def Test_interpreterCommand_For_6(): Unit = {
    val cube: Program =
      Progr(
        List(Var("X")),
        List(
          For(
            VarExp("X"),
            List(
              For(
                VarExp("X"),
                List(
                  Set(Var("X"), Cons(Nl, VarExp("X")))))))), /// valeur initiale de X : 2
        List(Var("X")))
    assertEquals(
      List(
        ConsValue(
          NlValue,
          ConsValue(
            NlValue,
            ConsValue(
              NlValue,
              ConsValue(
                NlValue,
                ConsValue(
                  NlValue,
                  ConsValue(
                    NlValue,
                    ConsValue(
                      NlValue,
                      ConsValue(NlValue, NlValue))))))))), // résulatat de cube(2) : 8
      interpreter(cube, List(ConsValue(NlValue, ConsValue(NlValue, NlValue)))))
  }
}
 