package com.example.easyreader3.presentation

enum class ItemType(val v: Int) {
    /*Перечисление видов айтемов для ресайкла в интерфейсе */
    USER(0),
    MENU(1),
   TEXT(2);
//    ITEM_WORD(1),
//    ITEM_NEW_WORD(2),

//    ITEM_TEXT_BLOCK(5),

    fun value() = v
}