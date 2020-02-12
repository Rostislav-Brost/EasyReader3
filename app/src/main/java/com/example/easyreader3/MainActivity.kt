package com.example.easyreader3

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import androidx.fragment.app.FragmentManager
import androidx.fragment.app.FragmentTransaction
import androidx.navigation.NavController
import androidx.navigation.fragment.NavHostFragment
import androidx.navigation.ui.setupWithNavController
//import com.example.easyreader3.presentation.MainPresenter
import com.google.android.material.navigation.NavigationView
import kotlinx.android.synthetic.main.activity_main.*


class MainActivity : AppCompatActivity() ,Navigation{
    val navController:NavController? =null
    //val presenter=MainPresenter()

//    @InjectPresenter
//    lateinit var mainPresenter: MainPresenter
//
//    @ProvidePresenter
//    fun provideMainPresenter()=MainPresenter()

    override fun onCreate(savedInstanceState: Bundle?) {
       // val mainPresenter = MainPresenter()
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        val host: NavHostFragment = supportFragmentManager
            .findFragmentById(R.id.navFragment) as NavHostFragment? ?: return

        val navController = host.navController
        val sideBar = findViewById<NavigationView>(R.id.nav_view)
        sideBar?.setupWithNavController(navController)
      //  presenter.initFakeData()


    }

    override fun navigateToScreen(id: Int) {
        navController?.navigate(id)
    }
}
