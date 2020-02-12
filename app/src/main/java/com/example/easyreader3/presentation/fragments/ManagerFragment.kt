package com.example.easyreader3.presentation.fragments


import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Toast
import androidx.fragment.app.Fragment
import androidx.recyclerview.widget.LinearLayoutManager
import com.example.easyreader3.R
import com.example.easyreader3.presentation.adapters.base.BaseAdapterCallback
import com.example.easyreader3.classes.User
import com.example.easyreader3.presentation.adapters.UserAdapter
import kotlinx.android.synthetic.main.include_recyclerview.*


class ManagerFragment : Fragment(){//MvpFragment() , {
    val users = ArrayList<User>()
    val userAdapter = UserAdapter()
      //    private val presenter by ProfileManagerPresenter { ProfileManagerPresenter.get() }

//    @InjectPresenter
//    lateinit var profileManagerPresenter: ProfileManagerPresenter
//
//    @ProvidePresenter
//    fun provideProfileManagerPresenter():ProfileManagerPresenter{
//        return ProfileManagerPresenter(listOf<User>())
//    }

//
//    override fun  setDataList(items: List<User>) {
//        users.clear()
//        users.addAll(items )
//        userAdapter.setList(users)
//    }
//
//    override fun showDialog(text: String, answer1: String, answer2: String) {}
//
//    override fun showToast(text: String) { }
//
//    override fun startWaiting() {}
//
//    override fun endWaiting() {  }
//
//



    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        users.addAll(
            listOf(
                User(
                    name = "Ivan Ivanov",
                    email = "i.i@now.ru",
                    age = 45
                ),
                User(
                    name = "Anna Romanova",
                    email = "anna.romanova@mail.ru",
                    age = 23
                ),
                User(
                    name = "Ivan Korchagin",
                    email = "kokokokorch@gmail.com",
                    age = 29
                ),
                User(
                    name = "Михаил",
                    email = "m.markelov@gmail.com",
                    age = 23
                )

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
      //  sm_btn_registration.setOnClickListener { profileManagerPresenter.btnreturn() }
    }

    override fun onDestroyOptionsMenu() {
        userAdapter.detachCallback()
        super.onDestroyOptionsMenu()
    }




}
