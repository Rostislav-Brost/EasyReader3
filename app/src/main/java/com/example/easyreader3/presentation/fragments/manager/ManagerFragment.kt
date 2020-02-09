package com.example.easyreader3.presentation.fragments.manager


import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.bases.BaseAdapterCallback
import com.example.easyreader3.bases.BaseFragment
import kotlinx.android.synthetic.main.include_recyclerview.*


class ManagerFragment : BaseFragment() {
    val users = ArrayList<User>()
    val userAdapter = UserAdapter()

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        users.addAll(
            listOf(
                User(name = "Ivan Ivanov", email = "i.i@now.ru", age = 45),
                User(name = "Anna Romanova", email = "anna.romanova@mail.ru", age = 23),
                User(name = "Ivan Korchagin", email = "kokokokorch@gmail.com", age = 29),
                User(name = "Михаил", email = "m.markelov@gmail.com", age = 23)

            )
        )
        userAdapter.attachCallback(object :
            BaseAdapterCallback<User> {
            override fun onItemClick(model: User, view: View) {
                Toast.makeText(activity, "Clicked", Toast.LENGTH_SHORT).show()
            }

            override fun onLongClick(model: User, view: View): Boolean {
                Toast.makeText(activity, "LongClicked", Toast.LENGTH_SHORT).show()
                return true
            }
        })
        return inflater.inflate(R.layout.fragment_manager, container, false)
    }

    override fun onStart() {
        rv.layoutManager = LinearLayoutManager(activity)
        userAdapter.setList(users)
        rv.adapter = userAdapter
        super.onStart()
    }

    override fun onDestroyOptionsMenu() {
        userAdapter.detachCallback()
        super.onDestroyOptionsMenu()
    }


}
