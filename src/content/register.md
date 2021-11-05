# Register

<form>
  <div class="mb-3">
    <label for="inputNick" class="form-label">Nick</label>
    <input type="text" class="form-control" id="inputNick" aria-describedby="nickHelp">
    <div id="nickHelp" class="form-text">Who are u ?</div>
  </div>
  <div class="mb-3">
    <label for="inputEmail" class="form-label">Email address</label>
    <input type="email" class="form-control" id="inputEmail" aria-describedby="emailHelp">
    <div id="emailHelp" class="form-text">We'll never share your email with anyone else.</div>
  </div>
  <div class="mb-3">
    <label for="inputPassword" class="form-label">Password</label>
    <input type="password" class="form-control" id="inputPassword">
  </div>
  <button type="submit" class="btn btn-primary" formaction="/register">Submit</button>
</form>
