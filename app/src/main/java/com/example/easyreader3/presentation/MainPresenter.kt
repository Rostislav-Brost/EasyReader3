package com.example.easyreader3.presentation



//@InjectViewState
//class MainPresenter(): MvpPresenter<MainActivityView>() {
//    private val TAG=MainPresenter::class.java.simpleName
//
//
//    val model = Model()
//    var users: List<User> = emptyList()
//    var currentUser: User = User()
//    var config=""
//
//    fun initFakeData(){
//        users =HardData().getFakeUsers(7)
//        currentUser=users[0]
//
//    }
//
//fun startScreen(){}
//    fun initData() {}
//
//
//}
//fun sortDictionary(input:List<Word>):List<Word> = input.sortedByDescending(){it.counter}
//fun inDictionary(inputword:String, a:ArrayList<Word>):Int{
//    var r:Int= -1
//    for(i in 0..a.lastIndex){if (a[i].w==inputword){r=i} }
//    return r }
//    fun makeDictionary( inputstring:String):ArrayList<Word>{
//        val delimiters=".,!#$%^&|*()_—=-+?><:;1234567890”`"
//        var  r=""
//
//        for(i in inputstring)if(!delimiters.contains(i)){ r+=i }
//        r=r.toLowerCase()
//        var wordArray= r.split(" ")
//        for(i in wordArray){
//            val index=inDictionary(i)
//            if(index!=-1){
//                a[index].counter++
//            }else{
//                if(!globaldictionary.contains(i)){a.add(Word(i))}
//            }
//        }
//        a.removeAll(){it.w==""}
//        return a
//    }