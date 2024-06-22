//#include "pch.h"
#include <stdlib.h>
#include <gtk/gtk.h>

#include <iostream>
#include <string>

#include <fstream>
#include <sstream>
#include "/home/josue/Documents/GitHub/ProyectoLenguajesDeProgramacion/ProyectoLenguajes/Evaluador.cpp"
///home/josue/Documents/GitHub/ProyectoLenguajesDeProgramacion/ProyectoLenguajes
#include <gtk/gtk.h>
#include <gtest/gtest.h>

using namespace std;



Evaluador Eval;


TEST(TestSuccess1, TestInfijaPostfijaS1) {
  EXPECT_EQ("13, 2, +", Eval.infijaAPostfija("13+2"));
  EXPECT_TRUE(true);
}

TEST(TestSuccess2, TestEvaluarPostfijaS2) {
	EXPECT_EQ(15, Eval.evaluarPostfija("12,3,+"));
	EXPECT_TRUE(true);
}

TEST(TestSuccess3, TestEvaluarPostfijaS3) {
	EXPECT_EQ(46.2, Eval.evaluarPostfija("15.4,3,*"));
	EXPECT_TRUE(true);
}

TEST(TestSuccess4, TestEvaluarInfijaS4) {
	EXPECT_EQ(true, Eval.evaluarInfija("180+2*3"));
	EXPECT_TRUE(true);
}

TEST(TestSuccess5, TestEvaluarInfijaS5) {
	EXPECT_EQ(true, Eval.evaluarInfija("3+532.6"));
	EXPECT_TRUE(true);
}

TEST(TestSuccess6, TestResultadoInfijaS6) {
	EXPECT_EQ(true, Eval.evaluarInfija("10 + 5 * (2 - 7) / (3 + 2) * 4 / 5"));
	EXPECT_TRUE(true);
}

TEST(TestSuccess7, TestEvaluarInfijaS7) {
	EXPECT_EQ(true, Eval.evaluarInfija("(231+5)"));
	EXPECT_TRUE(true);
}

TEST(TestSuccess8, TestEvaluarInfijaS8) {
	EXPECT_EQ(true, Eval.evaluarInfija("(231+5) + 3+ (19831+2)"));
	EXPECT_TRUE(true);
}


TEST(TestSuccess9, TestEvaluarInfijaS9) {
	EXPECT_EQ(false, Eval.evaluarInfija("(231+5) + 3+ (19831+2"));
	EXPECT_TRUE(true);
}


TEST(TestSuccess10, TestEvaluarInfijaS10) {
	EXPECT_EQ(true, Eval.evaluarInfija("192/3 + 3"));
	EXPECT_TRUE(true);
}

//------------------------------------------------------------------------------------------------------------------

TEST(TestFailed1, TestInfijaPostfijaF1) {
	EXPECT_EQ("10, 2, 2, 2, +, -, +", Eval.infijaAPostfija("10+2-(2+2)"));
	EXPECT_TRUE(true);
}

TEST(TestFailed2, TestInfijaPostfijaF2) {
	EXPECT_EQ("3, 10, 2, 2, +, -, +", Eval.infijaAPostfija("10+3-(2+2)"));
	EXPECT_TRUE(true);
}

TEST(TestFailed3, TestEvaluarInfijaF3) {
	EXPECT_EQ(true, Eval.evaluarInfija("(231+5"));
	EXPECT_TRUE(true);
}

TEST(TestFailed4, TestEvaluarInfijaF4) {
	EXPECT_EQ(true, Eval.evaluarInfija("-(265.5+367675.24)"));
	EXPECT_TRUE(true);
}

TEST(TestFailed5, TestEvaluarInfijaF5) {
	EXPECT_EQ(true, Eval.evaluarInfija("12++3+2"));
	EXPECT_TRUE(true);
}

TEST(TestFailed6, TestEvaluarInfijaF6) {
	EXPECT_EQ(false, Eval.evaluarInfija("(212+88)"));
	EXPECT_TRUE(true);
}

TEST(TestFailed7, TestEvaluarInfijaF7) {
	EXPECT_EQ("10, 5, 2, 7, -, , 32, +, /, 4, *, 5, /+", Eval.infijaAPostfija("10+5(2-7)/(3+2)*4/5"));
	EXPECT_TRUE(true);
}

TEST(TestFailed8, TestEvaluarInfijaF8) {
	EXPECT_EQ(92, Eval.evaluarPostfija("88, 122, 2, /, +, 13, -"));
	EXPECT_TRUE(true);
}

TEST(TestFailed9, TestEvaluarInfijaF9) {
	EXPECT_EQ(false, Eval.evaluarInfija("88+122/2-(13)"));
	EXPECT_TRUE(true);
}

TEST(TestFailed10, TestEvaluarInfijaF10) {
	EXPECT_EQ(true, Eval.evaluarInfija("((((((((("));
	EXPECT_TRUE(true);
}
