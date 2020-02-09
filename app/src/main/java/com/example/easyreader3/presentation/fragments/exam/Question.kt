package com.example.easyreader3.presentation.fragments.exam

/*класс вопроса содержит
* заголовок
* текст
*ПОЗЖЕ  url картинки
* список вариантов ответов
*ПОЗЖЕ тип вопроса (один ответ, диаппазон )
* правильные варианты ответа
*
* методы
* isRight(string)
* */
class Question(
    val title: String = "Question",
    val text: String = "",
    val imgUrl: String = "",
    val answers: List<String> = listOf("answer1", "answer2"),
    val rightAnswers: List<String> = listOf("answer1"),
    var lastAnswer: String = ""
) {
    fun isRight(answer: String): Boolean = rightAnswers.contains(answer)
}