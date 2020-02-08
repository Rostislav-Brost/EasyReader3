package com.example.easyreader3.presentation

class Item(
/*Универсальный класс элемента списка в инте*/
    val id: Int = 0,
    val title: String = "title",
    val text: String = "text1",
    val text2: String = "text2",
    val text3: String = "text3",
    val info: String = "info",
    val type: ItemType = ItemType.MENU

) {
    //дописать!!!
    fun user(
        name: String = "Default Name",
        email: String = "default@email.com",
        age: Int = 21
    ) =
        Item(
            title = name,
            text = email,
            info = "(" + age + ")",
            type = ItemType.USER
        )
//    fun word(
//        word: String,
//        translate: String = "",
//        transcribtion: String = "",
//        example: String = "",
//        counter: String = "nulll"
//    ) = Item(
//        title = word,
//        text = translate,
//        text2 = transcribtion,
//        text3 = example,
//        info = counter,
//        type = ItemType.ITEM_WORD
//    )
//
//    fun newWord(
//        word: String,
//        translate: String = "",
//        counter: String = "()"
//    ) = Item(
//        title = word,
//        text = translate,
//        info = counter,
//        type = ItemType.ITEM_WORD
//    )
//
//    fun text(
//        title: String,
//        text: String = "...",
//        info: String = "()"
//    ) =
//        Item(
//            title = title,
//            text = text,
//            info = info,
//            type = ItemType.ITEM_TEXT
//        )
//
//
//
//    fun menu(
//        text: String
//    ) =
//        Item(
//            title = text,
//            type = ItemType.ITEM_MENU
//        )
/*

    fun value() = v*/
    /*
    * профиль
    * заголовок( имя, фамилия , условный уровень)
    * аватар
    * уровень
    *
    * элементы меню
    * заголовок

    * * слова в пользовательском словаре
    * слово
    * перевод
    * встречено раз
    *
    * текст на экране текстов
    * заголовок
    * первая строка
    * цифра показателей (новых слов на количество слов)

    *
    *Данный класс  объединяет в себе все
    *  айтемы используемые в приложении.
    * в нем будут либо методы, либо конструкторы, создающий каждый тип айтемов
    * в адаптере по свитчу будет подбираться под них XML  файлы
    * в адаптер на вход будет подаваться список айтемов  и тип  файла ресурсов, в котором его показать
    * */
    //нужно создать интерфейс айтема и наследовать от него  все айтемы  в приложении

}
