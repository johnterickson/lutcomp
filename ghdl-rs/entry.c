static void my_handle_register(void)
{
    printf("HELLO FROM my_handle_register");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};