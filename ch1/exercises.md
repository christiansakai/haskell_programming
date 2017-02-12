# Equivalence Exercises

1. 𝜆𝑥𝑦.𝑥𝑧

  𝜆𝑥.𝜆𝑦.𝑥𝑧
  
  b) 𝜆𝑚𝑛.𝑚𝑧

  𝜆𝑚.𝜆𝑛.𝑚𝑧

2. 𝜆𝑥𝑦.𝑥𝑥𝑦

  𝜆𝑥.𝜆𝑦.𝑥𝑥𝑦

  c) 𝜆𝑎(𝜆𝑏.𝑎𝑎𝑏)

  𝜆𝑎.𝜆𝑏.𝑎𝑎𝑏

3. 𝜆𝑥𝑦𝑧.𝑧𝑥

  𝜆𝑥.𝜆𝑦.𝜆𝑧.𝑧𝑥

  b) 𝜆𝑡𝑜𝑠.𝑠𝑡

  𝜆𝑡.𝜆𝑜.𝜆𝑠.𝑠𝑡

# Exercises

__Combinators__. Determine if each of the following are combinators or not:

* 𝜆𝑥.𝑥𝑥𝑥 __Combinators__

* 𝜆𝑥𝑦.𝑧𝑥 __Not Combinators__

* 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥) __Combinators__

* 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦) __Combinators__

* 𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦) __Not Combinators__

__Normal form or diverge?__ Determine if each of the following can be reduced to a normal form of if they diverge:

* 𝜆𝑥.𝑥𝑥𝑥 __Normal__

* (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦) __Diverge__

* (𝜆𝑥.𝑥𝑥𝑥)𝑧 __Normal__

__Beta reduce__. Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.

1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)

  (𝜆𝑎.𝜆𝑏.𝜆𝑐.𝑐𝑏𝑎)(𝑧)(𝑧)(𝜆𝑤𝑣.𝑤)

  ([𝑎∶=𝑧].𝜆𝑏.𝜆𝑐.𝑐𝑏𝑎)(𝑧)(𝜆𝑤𝑣.𝑤)

  (𝜆𝑏.𝜆𝑐.𝑐𝑏𝑧)(𝑧)(𝜆𝑤𝑣.𝑤)

  ([𝑏:=𝑧].𝜆𝑐.𝑐𝑏𝑧)(𝜆𝑤𝑣.𝑤)

  (𝜆𝑐.𝑐𝑧𝑧)(𝜆𝑤𝑣.𝑤)

  (𝜆𝑐.𝑐𝑧𝑧)(𝜆𝑤.𝜆𝑣.𝑤)

  ([𝑐:=(𝜆𝑤.𝜆𝑣.𝑤)].𝑐𝑧𝑧)

  ((𝜆𝑤.𝜆𝑣.𝑤)𝑧𝑧)

  (𝜆𝑤.𝜆𝑣.𝑤)(𝑧)(𝑧)

  ([𝑤:=𝑧].𝜆𝑣.𝑤)(𝑧)

  (𝜆𝑣.𝑧)(𝑧)

  ([𝑣:=𝑧].𝑧)

  𝑧

2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏

  ([𝑥:=(𝜆𝑎.𝑎)].𝜆𝑦.𝑥𝑦𝑦)𝑏

  (𝜆𝑦.(𝜆𝑎.𝑎)𝑦𝑦)𝑏

  ([𝑦:=𝑏].(𝜆𝑎.𝑎)𝑦𝑦)

  (𝜆𝑎.𝑎)𝑏𝑏

  (𝜆𝑎.𝑎)(𝑏)(𝑏)

  ([𝑎:=𝑏].𝑎)(𝑏)

  𝑏𝑏

3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)

  ([𝑦:=(𝜆𝑥.𝑥𝑥)].𝑦)(𝜆𝑧.𝑧𝑞)

  (𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)

  ([𝑥:=(𝜆𝑧.𝑧𝑞)].𝑥𝑥)

  (𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞)

  ([𝑧:=(𝜆𝑧.𝑧𝑞)].𝑧𝑞)

  (𝜆𝑧.𝑧𝑞)𝑞

  ([𝑧:=𝑞].𝑧𝑞)

  𝑞𝑞

4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)

  ([𝑧:=(𝜆𝑧.𝑧𝑧)].𝑧)(𝜆𝑧.𝑧𝑦)

  (𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)

  ([𝑧:=(𝜆𝑧.𝑧𝑦)].𝑧𝑧)

  (𝜆𝑧.𝑧𝑦)(𝜆𝑧.𝑧𝑦)

  ([𝑧:=(𝜆𝑧.𝑧𝑦)].𝑧𝑦)

  (𝜆𝑧.𝑧𝑦)𝑦

  ([𝑧:=𝑦].𝑧𝑦)

  𝑦𝑦

5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦

  ([𝑥:=(𝜆𝑦.𝑦)].𝜆𝑦.𝑥𝑦𝑦)𝑦

  (𝜆𝑦.(𝜆𝑦.𝑦)𝑦𝑦)𝑦

  ([𝑦:=𝑦].(𝜆𝑦.𝑦)𝑦𝑦)

  (𝜆𝑦.𝑦)𝑦𝑦

  (𝜆𝑦.𝑦)(𝑦)(𝑦)

  ([𝑦:=𝑦].𝑦)(𝑦)

  𝑦𝑦

6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐

  ([𝑎:=(𝜆𝑏.𝑏𝑎)].𝑎𝑎)𝑐

  (𝜆𝑏.𝑏𝑎)(𝜆𝑏.𝑏𝑎)𝑐

  ([𝑏:=(𝜆𝑏.𝑏𝑎)].𝑏𝑎)𝑐

  ((𝜆𝑏.𝑏𝑎)𝑎)𝑐

  (([𝑏:=𝑎].𝑏𝑎))𝑐

  𝑎𝑎𝑐

7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)

  (𝜆𝑥.𝜆𝑦.𝜆𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)

  ([𝑥:=(𝜆𝑥.𝑧)].𝜆𝑦.𝜆𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑎)

  (𝜆𝑦.𝜆𝑧.(𝜆𝑥.𝑧)𝑧(𝑦𝑧))(𝜆𝑥.𝑎)

  ([𝑦:=(𝜆𝑥.𝑎)].𝜆𝑧.(𝜆𝑥.𝑧)𝑧(𝑦𝑧))

  (𝜆𝑧.(𝜆𝑥.𝑧)𝑧((𝜆𝑥.𝑎)𝑧))

  (𝜆𝑧.([𝑥:=𝑧].𝑧)((𝜆𝑥.𝑎)𝑧))

  (𝜆𝑧.𝑧((𝜆𝑥.𝑎)𝑧))

  (𝜆𝑧.𝑧(([𝑥:=𝑧].𝑎)))

  𝜆𝑧.𝑧𝑎

  This is wrong, because the `z` variable in the head and in the body is different. Next time, it is better to assign the starting lambda expression different variables, so the end result is not confusing.

  𝜆𝑧1.𝑧𝑎
