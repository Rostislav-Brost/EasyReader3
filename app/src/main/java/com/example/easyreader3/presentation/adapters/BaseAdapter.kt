package com.example.easyreader3.presentation.adapters

import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import java.util.*

class BaseAdapter : RecyclerView.Adapter<ItemAdapter.ItemHolder>() {
    private val items: MutableList<BaseItem> = LinkedList()

    fun setList(data: List<BaseItem>) {
        items.clear()
        items.addAll(data)
        notifyDataSetChanged()
    }
    fun addItem(item: BaseItem, position: Int) {
        items.add(position, item)
        notifyItemInserted(position)
    }
    fun addItems(itemsForAdd: List<BaseItem>, position: Int=0) {
        items.addAll(position,itemsForAdd)
        notifyItemRangeInserted(position,itemsForAdd.count())
    }
    fun updateItem(position: Int, newItem: BaseItem) {
        items[position] = newItem
        notifyItemChanged(position)
    }
    fun deleteItem(item: BaseItem) {
        val position = items.indexOf(item)
        items.removeAt(position)
        notifyItemRemoved(position)
    }



    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ItemAdapter.ItemHolder {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun getItemCount(): Int = items.size

    override fun onBindViewHolder(holder: ItemAdapter.ItemHolder, position: Int) {

    }


}