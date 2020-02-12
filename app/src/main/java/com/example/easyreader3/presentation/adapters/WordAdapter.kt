package com.example.easyreader3.presentation.adapters

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.example.easyreader3.R
import com.example.easyreader3.classes.Word
import com.example.easyreader3.presentation.adapters.base.BaseAdapter
import com.example.easyreader3.presentation.adapters.base.BaseViewHolder
import kotlinx.android.synthetic.main.item_word.view.*


class WordAdapter(): BaseAdapter<Word>() {
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): BaseViewHolder<Word> {
      return ViewHolder(
          LayoutInflater.from(parent.context).inflate(R.layout.item_word, parent, false)
      )
    }

class ViewHolder(itemView: View): BaseViewHolder<Word>(itemView){
    override fun bind(model: Word) {
      itemView.iw_word.text=model.title()
        itemView.iw_transcribtion.text=model.text()
        itemView.iw_translate.text=model.text2()
        itemView.iw_counter.text=model.info()
        itemView.iw_example.text=model.imgURL()
    }
}
}