package com.example.easyreader3.presentation.fragments

import android.os.Bundle
import androidx.fragment.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.example.easyreader3.R
import com.example.easyreader3.presentation.Item
import com.example.easyreader3.presentation.adapters.ItemAdapter
import kotlinx.android.synthetic.main.include_recyclerview.*

class RegistrationFragment : Fragment() {


    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        val view = inflater.inflate(R.layout.fragment_registration, container, false)
        // Inflate the layout for this fragment
        var data = listOf<Item>(
            Item().user(
                name = "Ivan Ivanov",
                email = "i.i@now.ru",
                age = 45
            ),
            Item().user(
                name = "Anna Romanova",
                email = "anna.romanova@mail.ru",
                age = 23
            ),
            Item().user(
                name = "Ivan Korchagin",
                email = "kokokokorch@gmail.com",
                age = 29
            )
        )

//        rv.layoutManager = LinearLayoutManager(activity, RecyclerView.VERTICAL, false)
//        rv.adapter = ItemAdapter(
//            data,
//            object : ItemAdapter.Callback {
//                override fun onItemClicked(item: Item) {
//                    itemClicked(item)
//                    //TODO Сюда придёт элемент, по которому кликнули. Можно дальше с ним работать
//                }
//            })
        return view
        /*  final LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mRecyclerView.setLayoutManager(layoutManager);



        mListadapter = new ListAdapter(data);
        mRecyclerView.setAdapter(mListadapter);
*/
    }

    private fun itemClicked(item: Item) {

    }
}