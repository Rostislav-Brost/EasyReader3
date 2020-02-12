package com.example.easyreader3.presentation.adapters

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.example.easyreader3.R
import com.example.easyreader3.presentation.adapters.base.BaseAdapter
import com.example.easyreader3.presentation.adapters.base.BaseViewHolder
import com.example.easyreader3.classes.Book
import kotlinx.android.synthetic.main.item_string.view.*

class StringAdapter() :
    BaseAdapter<String>() {
    val MAX_STRING_SIZE = 50
    val HEADER_ID = 0
    val FOOTER_ID = 1
    val QUESTION_ID = 2

    fun setList(data: Book) {
        items.clear()
        var forAdd = ""
        for (i in data.text.split(" ")) if ((forAdd +" "+ i).length < MAX_STRING_SIZE) {
            forAdd = forAdd+ " "+ i
        } else {
            items.add(" "+forAdd)
            forAdd = " " + i
        }
        items.add(forAdd)
        hasItems = true
        notifyDataSetChanged()
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int) =
        ViewHolder(
            LayoutInflater.from(parent.context).inflate(R.layout.item_string, parent, false)
        )


    class ViewHolder(itemView: View) : BaseViewHolder<String>(itemView) {
        override fun bind(model: String) {
            itemView.is_string.text = model

        }
    }

}