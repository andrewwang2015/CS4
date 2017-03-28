let returnInitialStorage init =
  let newStorage = Queue.create() in
  let x = Queue.add [init] newStorage in newStorage
