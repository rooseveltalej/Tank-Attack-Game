import pygame
import random
import subprocess
import threading
    
# Inicialización de Pygame
pygame.init()

# Configuración de pantalla
SCREEN_WIDTH = 800
SCREEN_HEIGHT = 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Juego de Tanques")

# Colores
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)

# Tamaño de los bloques (tanques, muros)
BLOCK_SIZE = 40

# Cargar imágenes
player_image = pygame.image.load('images/tank_player.png')
enemy_image = pygame.image.load('images/tank_enemy.png')
wall_image = pygame.image.load('images/wall.png')
object_enemy = pygame.image.load('images/object_enemy.png') ##Estos son los objetos que vamos a elimninar, una vez se eliminan el juego termina
# Cargar imagen de fondo
background_image = pygame.image.load('images/background.png')
# Ajustar el tamaño del fondo a la pantalla
background_image = pygame.transform.scale(background_image, (SCREEN_WIDTH, SCREEN_HEIGHT))


# Escalar imágenes si es necesario
player_image = pygame.transform.scale(player_image, (BLOCK_SIZE, BLOCK_SIZE))
enemy_image = pygame.transform.scale(enemy_image, (BLOCK_SIZE, BLOCK_SIZE))
wall_image = pygame.transform.scale(wall_image, (BLOCK_SIZE, BLOCK_SIZE))
object_enemy_image = pygame.transform.scale(object_enemy, (BLOCK_SIZE, BLOCK_SIZE))

clock = pygame.time.Clock()

# El juego soporta una matriz de 20x15 bloques
MAP = [
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
]


# --- CLASES ---

class Tank:
    def __init__(self, x, y, image, speed=5, lives=3):  # Agregar parámetro lives
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)
        self.image = image
        self.original_image = image
        self.speed = speed
        self.direction = 'UP'
        self.lives = lives  # Atributo de vidas

    def draw(self, screen):
        # Dibujar la imagen en lugar del rectángulo
        screen.blit(self.image, self.rect)

    def move(self, x_change, y_change, walls):
        # Nueva posición propuesta
        new_rect = self.rect.move(x_change, y_change)

        # Comprobar los límites de la pantalla
        if new_rect.left < 0:  # Límite izquierdo
            new_rect.left = 0
        elif new_rect.right > SCREEN_WIDTH:  # Límite derecho
            new_rect.right = SCREEN_WIDTH
        
        if new_rect.top < 0:  # Límite superior
            new_rect.top = 0
        elif new_rect.bottom > SCREEN_HEIGHT:  # Límite inferior
            new_rect.bottom = SCREEN_HEIGHT

        # Verificar colisión con muros
        for wall in walls:
            if new_rect.colliderect(wall.rect):
                return  # No moverse si choca con un muro

        # Actualizar la posición si no hay colisión
        self.rect = new_rect

            

    def rotate_image(self):
        """ Rota la imagen del tanque según la dirección """
        if self.direction == 'UP':
            self.image = pygame.transform.rotate(self.original_image, 0)  # Imagen original
        elif self.direction == 'DOWN':
            self.image = pygame.transform.rotate(self.original_image, 180)
        elif self.direction == 'LEFT':
            self.image = pygame.transform.rotate(self.original_image, 90)
        elif self.direction == 'RIGHT':
            self.image = pygame.transform.rotate(self.original_image, -90)

class Wall:
    def __init__(self, x, y):
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)

    def draw(self, screen):
        # Dibujar la imagen del muro
        screen.blit(wall_image, self.rect)


class EnemyTank(Tank):
    def __init__(self, x, y, image, speed=5):
        super().__init__(x, y, image, speed)
        self.path = []
        self.target_pos = None
        self.last_shot_time = 0
        self.shoot_interval = 2000

    def update(self, player_pos, walls, bullets):
        if not self.path:
            threading.Thread(self.calculate_path(player_pos, walls)).start()

        if self.path:
            if self.target_pos is None or self.rect.topleft == self.target_pos:
                self.target_pos = self.path.pop(0)

            self.move_towards_target()

        self.rotate_image()
        self.shoot_if_needed(bullets)

    def move_towards_target(self):
        if self.target_pos:
            target_x, target_y = self.target_pos
            current_x, current_y = self.rect.topleft

            if current_x < target_x:
                self.rect.x += self.speed
                self.direction = 'RIGHT'
            elif current_x > target_x:
                self.rect.x -= self.speed
                self.direction = 'LEFT'

            if current_y < target_y:
                self.rect.y += self.speed
                self.direction = 'DOWN'
            elif current_y > target_y:
                self.rect.y -= self.speed
                self.direction = 'UP'

    def calculate_path(self, player_pos, walls):
        with open('./app/input.txt', 'w') as f:
            f.write(f"{self.rect.x // BLOCK_SIZE} {self.rect.y // BLOCK_SIZE} {player_pos[0] // BLOCK_SIZE} {player_pos[1] // BLOCK_SIZE}\n")
            for wall in walls:
                f.write(f"{wall.rect.x // BLOCK_SIZE} {wall.rect.y // BLOCK_SIZE} ")

        with open('./app/input.txt', 'r') as f:
            print("Contenido del archivo de entrada:", f.read())

        result = subprocess.run(['runhaskell', './app/Main.hs'], capture_output=True, text=True)
        print("Resultado de Haskell:", result.stdout)
        if result.returncode != 0:
            print("Error en la ejecución de Haskell:", result.stderr)

        try:
            with open('./app/output.txt', 'r') as f:
                path = f.read().strip().split()
                print("Ruta obtenida de Haskell:", path)
                self.path = [tuple(map(int, pos.strip('()').split(','))) for pos in path]
                self.path = [(x * BLOCK_SIZE, y * BLOCK_SIZE) for x, y in self.path]
        except FileNotFoundError:
            print("Archivo de salida no encontrado.")

    def rotate_image(self):
        if self.direction == 'UP':
            self.image = pygame.transform.rotate(self.original_image, 0)
        elif self.direction == 'DOWN':
            self.image = pygame.transform.rotate(self.original_image, 180)
        elif self.direction == 'LEFT':
            self.image = pygame.transform.rotate(self.original_image, 90)
        elif self.direction == 'RIGHT':
            self.image = pygame.transform.rotate(self.original_image, -90)

    def shoot_if_needed(self, bullets):
        current_time = pygame.time.get_ticks()
        if current_time - self.last_shot_time > self.shoot_interval:
            self.shoot(bullets)
            self.last_shot_time = current_time

    def shoot(self, bullets):
        if self.direction == 'LEFT':
            bullet_x = self.rect.left
            bullet_y = self.rect.centery
        elif self.direction == 'RIGHT':
            bullet_x = self.rect.right
            bullet_y = self.rect.centery
        elif self.direction == 'UP':
            bullet_x = self.rect.centerx
            bullet_y = self.rect.top
        elif self.direction == 'DOWN':
            bullet_x = self.rect.centerx
            bullet_y = self.rect.bottom

        bullet = Bullet(bullet_x, bullet_y, self.direction, is_enemy_bullet=True)
        bullets.append(bullet)

    def move(self, x_change, y_change, walls):
        new_rect = self.rect.move(x_change, y_change)
        if new_rect.left < 0:
            new_rect.left = 0
        elif new_rect.right > SCREEN_WIDTH:
            new_rect.right = SCREEN_WIDTH
        if new_rect.top < 0:
            new_rect.top = 0
        elif new_rect.bottom > SCREEN_HEIGHT:
            new_rect.bottom = SCREEN_HEIGHT
        for wall in walls:
            if new_rect.colliderect(wall.rect):
                return
        self.rect = new_rect


class Bullet:
    def __init__(self, x, y, direction, is_enemy_bullet=False):
        self.rect = pygame.Rect(x, y, 5, 5)
        self.speed = 10
        self.direction = direction  # ('UP', 'DOWN', 'LEFT', 'RIGHT')
        self.is_enemy_bullet = is_enemy_bullet  # Indica si es una bala enemiga

    def move(self):
        if self.direction == 'UP':
            self.rect.y -= self.speed
        elif self.direction == 'DOWN':
            self.rect.y += self.speed
        elif self.direction == 'LEFT':
            self.rect.x -= self.speed
        elif self.direction == 'RIGHT':
            self.rect.x += self.speed

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.rect)

    def off_screen(self):
        return (self.rect.x < 0 or self.rect.x > SCREEN_WIDTH or
                self.rect.y < 0 or self.rect.y > SCREEN_HEIGHT)


# --- FUNCIONES ---
def generate_walls(map_data):
    """Generar muros según el mapa"""
    walls = []
    for row in range(len(map_data)):
        for col in range(len(map_data[row])):
            if map_data[row][col] == 1:
                wall = Wall(col * BLOCK_SIZE, row * BLOCK_SIZE)
                walls.append(wall)
    return walls

class ObjectEnemy:
    def __init__(self, x, y, image):
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)
        self.image = image

    def draw(self, screen):
        screen.blit(self.image, self.rect)

    def check_collision(self, bullets):
        # Verificar si ha sido alcanzado por una bala
        for bullet in bullets:
            if self.rect.colliderect(bullet.rect):
                if not bullet.is_enemy_bullet:  # Solo eliminar si la bala no es enemiga
                    bullets.remove(bullet)  # Remover la bala
                    return True  # Indica que el objeto fue destruido
        return False



walls = generate_walls(MAP)


# --- FUNCIONES AUXILIARES ---

def handle_bullets(bullets, enemies, walls, player, object_enemies):
    for bullet in bullets[:]:
        bullet.move()

        # Verificar colisión con muros
        for wall in walls:
            if bullet.rect.colliderect(wall.rect):
                if bullet in bullets:
                    bullets.remove(bullet)
                break

        # Verificar colisión con el jugador
        if bullet.rect.colliderect(player.rect):
            if bullet.is_enemy_bullet:  # Solo reducir vidas si es una bala enemiga
                player.lives -= 1
                if player.lives <= 0:
                    print("¡Has perdido!")
                    pygame.quit()
                    return
            player.rect.topleft = (100, 40)  # Posición inicial del jugador
            if bullet in bullets:
                bullets.remove(bullet)
            break

        # Verificar colisión con enemigos
        for enemy in enemies[:]:  # Hacemos una copia de la lista de enemigos para eliminar sin problemas
            if bullet.rect.colliderect(enemy.rect) and not bullet.is_enemy_bullet:
                if bullet in bullets:
                    bullets.remove(bullet)  # Eliminar la bala
                enemies.remove(enemy)  # Eliminar el enemigo
                break

        # Verificar colisión con objetos enemigos
        for obj_enemy in object_enemies[:]:  # Copia de la lista para eliminar sin problemas
            if bullet.rect.colliderect(obj_enemy.rect):
                if not bullet.is_enemy_bullet:  # Solo eliminar si la bala no es enemiga
                    if bullet in bullets:
                        bullets.remove(bullet)  # Eliminar la bala
                    object_enemies.remove(obj_enemy)  # Eliminar el objeto enemigo
                    break

        if bullet.off_screen() and bullet in bullets:
            bullets.remove(bullet)


def draw_lives(screen, lives, x=10, y=10):
    font = pygame.font.Font(None, 36)
    text = font.render(f'Vidas: {lives}', True, WHITE)
    screen.blit(text, (x, y))
    

def generate_random_position(walls):
    while True:
        x = random.randint(0, (SCREEN_WIDTH // BLOCK_SIZE) - 1) * BLOCK_SIZE
        y = random.randint(0, (SCREEN_HEIGHT // BLOCK_SIZE) - 1) * BLOCK_SIZE
        new_rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)

        # Verificar si la nueva posición colisiona con algún muro
        if not any(new_rect.colliderect(wall.rect) for wall in walls):
            return (x, y)


# --- CONFIGURACIÓN INICIAL ---

player = Tank(100, 40, player_image)  # Tanque del jugador
#walls = [Wall(200, 200), Wall(240, 200)]  # Lista de muros
enemies = [EnemyTank(*generate_random_position(walls), enemy_image) for _ in range(5)]
object_enemies = [ObjectEnemy(*generate_random_position(walls + enemies), object_enemy_image) for _ in range(5)]  # Crear 5 object_enemy

bullets = []  # Lista de balas

# --- BUCLE PRINCIPAL ---
running = True
while running:
    keys = pygame.key.get_pressed()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

        # Disparar bala al presionar la barra espaciadora
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                # Crear una bala en la dirección actual del jugador
                if keys[pygame.K_UP]:
                    bullet = Bullet(player.rect.centerx, player.rect.top, 'UP')
                elif keys[pygame.K_DOWN]:
                    bullet = Bullet(player.rect.centerx, player.rect.bottom, 'DOWN')
                elif keys[pygame.K_LEFT]:
                    bullet = Bullet(player.rect.left, player.rect.centery, 'LEFT')
                elif keys[pygame.K_RIGHT]:
                    bullet = Bullet(player.rect.right, player.rect.centery, 'RIGHT')
                bullets.append(bullet)

    
    x_change, y_change = 0, 0

    if keys[pygame.K_UP]:
        y_change = -player.speed
        player.direction = 'UP'
    elif keys[pygame.K_DOWN]:
        y_change = player.speed
        player.direction = 'DOWN'
    elif keys[pygame.K_LEFT]:
        x_change = -player.speed
        player.direction = 'LEFT'
    elif keys[pygame.K_RIGHT]:
        x_change = player.speed
        player.direction = 'RIGHT'

    player.move(x_change, y_change, walls)
    player.rotate_image()

    # Dibujar todo primero
    screen.blit(background_image, (0, 0))

    # Dibujar muros
    for wall in walls:
        wall.draw(screen)

    # Dibujar al jugador y enemigos
    player.draw(screen)
    for enemy in enemies:
        enemy.draw(screen)

    # Dibujar los objetos enemigos
    for obj_enemy in object_enemies:
        obj_enemy.draw(screen)

    # Dibujar las balas
    for bullet in bullets:
        bullet.draw(screen)

    # Dibujar las vidas del jugador
    draw_lives(screen, player.lives)

    # Actualizar la pantalla
    pygame.display.flip()

    # Realizar el cálculo de la ruta con Haskell después de actualizar la pantalla
    for enemy in enemies:
        enemy.update(player.rect.center, walls, bullets)

    # Manejo de las balas y colisiones
    handle_bullets(bullets, enemies, walls, player, object_enemies)

    # Verificar si se han eliminado todos los object_enemy
    if len(object_enemies) == 0:
        print("¡Has ganado!")
        running = False  # Finalizar el juego si todos los objetos enemigos han sido eliminados

    # Controlar el FPS
    clock.tick(30)

    if player.lives == 0: 
        running = False

pygame.quit()