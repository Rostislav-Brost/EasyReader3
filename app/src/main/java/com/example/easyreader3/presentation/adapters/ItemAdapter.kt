package com.example.easyreader3.presentation.adapters

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import com.example.easyreader3.R
import com.example.easyreader3.presentation.Item
import com.example.easyreader3.presentation.ItemType
import kotlinx.android.synthetic.main.item_menu.view.*
import kotlinx.android.synthetic.main.item_text.view.*
import kotlinx.android.synthetic.main.item_user.view.*
import java.util.*


class ItemAdapter(val callback: Callback) : RecyclerView.Adapter<ItemAdapter.ItemHolder>(){
    private val items: MutableList<Item> = LinkedList()

//    fun deletebyName(name:String){
//        val position = items.indexOfFirst { it.title.equals(name,ignoreCase = true)}
//        if(position>=0){
//            items.removeAt(position)
//            notifyItemRemoved(position)
//        }
//    }

    interface Callback {
        fun onItemClicked(item: Item)
    }

    override fun getItemCount() = items.size
    // override fun getItemViewType(position: Int) = items[position].type.value()

    override fun onCreateViewHolder(vg: ViewGroup, viewType: Int) =
        ItemHolder(LayoutInflater.from(vg.context).inflate(R.layout.item_user, vg, false))


    override fun onBindViewHolder(holder: ItemHolder, position: Int) {
        holder.bind(items[position])
    }

    inner class ItemHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        fun bind(item: Item) {
            itemView.iu_name.text = item.title
            itemView.iu_email.text = item.text
            itemView.iu_age.text = item.info
            itemView.setOnClickListener {
                if (adapterPosition != RecyclerView.NO_POSITION) callback.onItemClicked(items[adapterPosition])
            }
        }
    }
}

