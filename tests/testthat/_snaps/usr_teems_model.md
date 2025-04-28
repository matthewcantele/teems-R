# teems_model validates input files correctly

    Code
      teems_model(tab_file = "nonexistent.tab")
    Condition
      Error in `teems_model()`:
      x Cannot open file 'nonexistent.tab': No such file.

---

    Code
      teems_model(tab_file = mock_tab)
    Condition
      Error in `teems_model()`:
      x Cannot open file '/tmp/Rtmp8dIjvo/file2dac8810df30e.txt': No such file.

