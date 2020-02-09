package com.example.easyreader3.presentation.fragments.manager

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.example.easyreader3.R
import com.example.easyreader3.bases.BaseAdapter
import com.example.easyreader3.bases.BaseViewHolder
import kotlinx.android.synthetic.main.item_user.view.*

class UserAdapter() : BaseAdapter<User>() {
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int) = ViewHolder(
        LayoutInflater.from(parent.context).inflate(R.layout.item_user, parent, false)
    )


    class ViewHolder(itemView: View) : BaseViewHolder<User>(itemView) {
        override fun bind(model: User) {
            itemView.iu_name.text = model.title()
            itemView.iu_email.text = model.text()
            itemView.iu_age.text = model.text2()
            //    itemView.iu_profile_img.text=model.imgURL()
        }
    }

}